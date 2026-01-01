// ╻ ╻┏━┓╺┳┓┏━╸╻  
// ┗┳┛┃ ┃ ┃┃┣╸ ┃  
//  ╹ ┗━┛╺┻┛┗━╸┗━╸
//  YAML Object Derivation & Expansion Language
//  version 0.1.0 | MIT License
//  Copyright (C) 2026 by Steven Gardiner <gardiner \at fnal.gov>
#pragma once

// Standard library includes
#include <cctype>
#include <cstdint>
#include <iostream>
#include <optional>
#include <regex>
#include <stdexcept>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

// fkYAML single-header library
// https://github.com/fktn-k/fkYAML
#include "fkYAML/node.hpp"

namespace yodel {

  // Specialized version of the fkYAML basic_node template. In particular,
  // the choice of fkyaml::ordered_map preserves the lexical order of the input
  using ordered_node = fkyaml::basic_node<
    std::vector, // sequence container
    fkyaml::ordered_map, // mapping container
    bool,
    std::int64_t,
    double,
    std::string,
    fkyaml::node_value_converter
  >;

namespace internal {

  // Constants defining meta keys used in YODEL. This block provides a single
  // location for easy editing to allow for future changes.
  inline constexpr char AUTO_LOCAL_PREFIX = '_';
  inline constexpr char PATH_DELIMITER = '.';

  inline const std::string TEMPLATE_PARAMETERS = "template parameters";
  inline const std::string LOCALS = "locals";
  inline const std::string DOC_ROOT = "root";
  inline const std::string ID = "id";
  inline const std::string AUTO_ID = AUTO_LOCAL_PREFIX + ID;
  inline const std::string LOCALS_KEYS = '_' + LOCALS + "_keys";
  inline const std::string OPEN_PLACEHOLDER = "{";
  inline const std::string CLOSE_PLACEHOLDER = "}";
  inline const std::string BASE = "base";
  inline const std::string BASE_KEYS = '_' + BASE + "_keys";
  inline const std::string OVERRIDES = "overrides";
  inline const std::string INSTANCE_FIELDS = "instance fields";
  inline const std::string OV_PER_INSTANCE = OVERRIDES + " per-instance";
  inline const std::string SWAPS = "swaps";
  inline const std::string WHEN = "when";
  inline const std::string REPLACE = "replace";
  inline const std::string VALUE_FROM = "value from";
  inline const std::string LITERAL_SWAPS = "_literal_" + SWAPS;

  // Index for managing YODEL object identities
  struct IdIndex {

    // scope_path -> (token -> canonical_identity)
    std::unordered_map< std::string,
      std::unordered_map< std::string, std::string > > table;

    // canonical_identity -> copy of concrete node (for clone)
    std::unordered_map< std::string, ordered_node > canonical_nodes;

    // parent_scope -> vector of child object scopes (canonical scope strings)
    std::unordered_map< std::string, std::vector< std::string > > children;
  
    static std::string parent_of( const std::string& scope );
  
    void register_child_scope( const std::string& parent_scope,
      const std::string& child_scope );

    void register_token( const std::string& scope, const std::string& token,
      const ordered_node& obj );
  
    bool has_canonical( const std::string& canon ) const;
  
    std::optional< std::string >
      resolve_qualified( const std::string& token ) const;
  
    // Unqualified tokens: single symbol, sibling-first ladder
    // with rung-local ambiguity
    std::string resolve_via_ladder( const std::string& start_scope,
      const std::string& token ) const;
  
    // Attempt a complete chain "[s1, s2, ...]" starting from base scope.
    // Returns canonical identity of the last segment
    // if the full chain exists; otherwise the return value is empty.
    std::optional< std::string > try_chain_once(
      const std::string& base_scope,
      const std::vector< std::string >& segs ) const;
  
    // Partially-scoped tokens: dotted, not starting with "root."
    std::string resolve_partial_chain( const std::string& start_scope,
      const std::vector<std::string>& segs ) const;

  };

} // namespace yodel::internal

  class Resolver {
  private:

    // Default maximum number of passes for the Resolve processing step
    static constexpr int DEFAULT_MAX_PASSES = 5;
    int max_bind_passes_ = DEFAULT_MAX_PASSES;
 
  public:
    // Constructor optionally takes a non-default number of Resolve passes
    // allowed during the Resolve step
    inline explicit Resolver( int bind_passes = DEFAULT_MAX_PASSES )
      : max_bind_passes_( bind_passes ), session_() {}
  
    // Concretize + Resolve YODEL --> plain YAML
    ordered_node resolve( std::istream& in );
    ordered_node resolve( const std::string& text );

  private:
 
    // Wraps internal state refreshed upon each call to resolve(...)
    struct ResolveSession {

      // Manages canonical identity indexing
      internal::IdIndex ids;
  
      // Root context (collected once)
      std::unordered_map< std::string, std::string > root_ctx;

      // Inherited context (nearest-wins)
      std::vector<
        std::unordered_map< std::string, std::string > > ctx_stack;
  
      // Tracks the path using sequence-element indexing,
      // e.g., ["root", "runs[0]", "files"]
      std::vector< std::string > path_stack;
      
      // Canonical scope, e.g., "root.runs.Run1"
      std::string scope_path;
  
      // Inline base cycle detection (per object chain)
      std::unordered_set<std::string> visited_bases_chain;
  
      // Overrides authored on templates, captured during Concretize
      // and applied later in Resolve (per instance canonical scope).
      struct OverridesForInstance {
        // mapping form: field -> [overlays] (sequence)
        ordered_node per_instance_map;
        // sequence of {when, replace} rules
        ordered_node per_instance_seq;
        // mapping applied to every instance
        ordered_node broadcast_map;
        // final overlay computed from per_instance_seq for a given instance
        ordered_node conditional_overlay;
  
        inline bool has_any() const;
      };

      // Keys are instance canonical identities
      std::unordered_map< std::string,
        OverridesForInstance > overrides_index;
    };
  
    // Current document object model (DOM) being processed
    ordered_node doc_;

    // Other internal state used during a call to resolve(...)
    ResolveSession session_;
  
    // Processing stages
    ordered_node parse_and_preflight( const std::string& yaml_text );
    void apply_section_overrides_in_place(); // operates on doc_
    void collect_index_and_concretize(); // operates on doc_ and session_
    void resolve_unified(); // operates on doc_ and session_
    void prune_final(); // operates on doc_
  
    // Signals how context should be built internally by
    // bind_strings_multi_pass(...) depending on the processing stage
    enum class BinderMode { Concretize, Resolve };
  
    void bind_strings_multi_pass( ordered_node& obj, BinderMode mode );
  
    // 'value from' materializer
    ordered_node materialize_value_from( const ordered_node& authored,
      const std::unordered_map< std::string, std::string >& bind_ctx,
      const char* usage_label, std::optional<std::string> required_shape,
      BinderMode mode );
  
    // Identity selection with policy (signals different behavior in
    // Concretize/Resolve steps)
    enum class IdentityPolicy { RequireLiteralInConcretize,
      AllowBoundLocalsInResolve };
  
    std::optional< std::string > element_identity_token(
      const ordered_node& el,
      const std::unordered_map< std::string, std::string >& ctx,
      IdentityPolicy policy );
  
    void apply_sequence_overlays_by_id( ordered_node& base_seq,
      const ordered_node& overlay_map );
  
    // Base resolver/merger
    void resolve_and_merge_base( ordered_node& target_map,
      const std::string& current_object_canon );
  
    // Swaps: bind map to literals, compose effective, shallow apply
    std::unordered_map< std::string, std::string >
      bind_swaps_map_to_literals( const ordered_node& swaps_map,
        const std::unordered_map<std::string, std::string>& ctx );
  
    void apply_literal_swaps_shallow( ordered_node& node,
      const std::unordered_map< std::string, std::string >& swaps );
  
    // Concretize uses params/local context only (intra-locals chaining)
    std::unordered_map< std::string, std::string >
    build_concretize_local_ctx( const ordered_node& obj_or_template,
      const std::unordered_map< std::string, std::string >& params );

    // Resolve uses inherited context. Nearest-wins behavior
    // handled using a stack
    void ctx_push( const ordered_node& mapping_fields );
    void ctx_pop();
  
    // Helper for building error messages
    [[noreturn]] void throw_error_at( const std::string& msg,
      const std::optional< std::string >& hint = std::nullopt );
  
  }; // class Resolver

namespace internal {

  // Classify an identity token
  enum class TokenKind {
    Qualified, // Starts with DOC_ROOT + PATH_DELIMITER, no ladder search
    Partial, // Includes section divisions (via PATH_DELIMITER), needs search
    Unqualified // No section divisions
  };
  
  inline TokenKind classify_token( const std::string& tok ) {
    if ( tok.rfind(DOC_ROOT + PATH_DELIMITER, 0) == 0 ) {
      return TokenKind::Qualified;
    }
    if ( tok.find(PATH_DELIMITER) != std::string::npos ) {
      return TokenKind::Partial;
    }
    return TokenKind::Unqualified;
  }
  
  // Divide an identity token string by PATH_DELIMITER instances
  inline std::vector< std::string > split_segments( const std::string& tok ) {
    std::vector< std::string > segs;
    size_t start = 0;
    while ( true ) {
      size_t pos = tok.find( PATH_DELIMITER, start );
      if ( pos == std::string::npos ) {
        segs.push_back( tok.substr(start) );
        break;
      }
      segs.push_back( tok.substr(start, pos - start) );
      start = pos + 1;
    }
    return segs;
  }
  
  // Connects identity tokens into a full path string with PATH_DELIMITER
  inline std::string join_path( const std::vector< std::string >& segs ) {
    std::string s;
    for ( size_t i = 0; i < segs.size(); ++i ) {
      if ( i ) s += PATH_DELIMITER;
      s += segs[ i ];
    }
    return s;
  }
  
  // Append a numerical index to the end of a base path string
  inline std::string seq_indexed( const std::string& base, size_t idx ) {
    return base + '[' + std::to_string( idx ) + ']';
  }
  
  // Helper that checks whether an ordered_node is a non-null scalar field
  inline bool is_non_null_scalar( const ordered_node& n ) {
    return ( n.is_scalar() && !n.is_null() );
  }

  // Helpers for conversions to/from the ordered_node type
  
  template < typename T >
  inline T to_native_checked( const ordered_node& n ) {
    T out;
    fkyaml::node_value_converter< T >::from_node( n, out );
    return out;
  }
  
  template < typename T >
  inline ordered_node make_node_from( const T& value ) {
    ordered_node n;
    fkyaml::node_value_converter< T >::to_node( n, value );
    return n;
  }
  
  inline std::string to_string_any( const ordered_node& n ) {
    if ( n.is_string() ) return to_native_checked< std::string >( n );
    if ( n.is_integer() ) return std::to_string(
      to_native_checked< std::int64_t >( n )
    );
    if ( n.is_boolean() ) return n.get_value< bool >() ? "true" : "false";
    if ( n.is_float_number() ) return std::to_string(
      to_native_checked< double >( n )
    );

    // We did not match any of the scalar types, so fall back to serialization
    return ordered_node::serialize( n );
  }

  // Deep merge of an overlay node onto a base node. Executes simple
  // replacement for scalars and sequences. An explicit null overlay clears
  // the corresponding base node. For an overlay and base that are both
  // mappings, deep merge the contents with an "overlay wins" policy.
  inline ordered_node deep_merge( const ordered_node& base,
    const ordered_node& overlay )
  {
    // An explicit null clears the corresponding prior entry
    if ( overlay.is_null() ) return ordered_node();

    // Non-mapping overlays (scalars and sequences) replace prior base values
    if ( !overlay.is_mapping() ) return overlay;

    // If the overlay is a mapping but the base isn't, the overlay replaces it
    if ( !base.is_mapping() ) return overlay;
  
    // Copy the base map
    ordered_node result = base;

    // Merge overlay keys into the copy (overlay wins on conflicts)
    for ( const auto& [mk, mv] : overlay.map_items() ) {
      const std::string k = mk.get_value< std::string >();
      if ( result.contains(k) ) {
        // Descend into child nodes as needed to complete the deep merge
        result[ k ] = deep_merge( result.at(k), mv );
      } else {
        result[ k ] = mv;
      }
    }
    return result;
  }
  
  // Helpers for rejection of YAML anchors/aliases in raw text input
  
  // Character classes for raw-text scan
  inline bool is_anchor_alias_name_char( char ch ) {
    unsigned char c = static_cast< unsigned char >( ch );
    return std::isalnum( c ) || c == '_' || c == '-';
  }
  
  // Characters that can precede the start of a YAML value token.
  inline bool is_boundary_left_char( char ch ) {
    switch ( ch ) {
      case ' ': case '\t': case '-': case ':':
      case '[': case '{': case ',':
        return true;
      default: return false;
    }
  }
  
  // Characters that can follow the end of an alias/anchor token.
  inline bool is_boundary_right_char( char ch ) {
    switch ( ch ) {
      case ' ': case '\t': case '\r': case '\n':
      case ',': case ']': case '}': case '#': case ':':
        return true;
      default: return false;
    }
  }
  
  // Reject anchor/alias tokens (&/*) at relevant boundaries in raw YAML
  // input (comments and quoted strings will be skipped)
  inline void preflight_reject_anchors_aliases( const std::string& text ) {
    bool in_single = false, in_double = false, in_comment = false;
    size_t line = 1, col = 0;
  
    for ( size_t i = 0; i < text.size(); ++i ) {
      char c = text[ i ]; col++;
  
      // Newline resets comment state and column
      if ( c == '\n' ) { in_comment = false; line++; col = 0; continue; }
  
      // Comment handling (outside quotes): '#' starts a comment until newline
      if ( !in_double && !in_single ) {
        if ( !in_comment && c == '#' ) { in_comment = true; continue; }
      }

      if ( in_comment ) continue;
  
      // Quote state machine
      if ( !in_double && c == '\'' ) { in_single = !in_single; continue; }
      if ( !in_single && c == '\"' ) { in_double = !in_double; continue; }
      if ( in_single || in_double ) continue;
  
      // Detect anchors/aliases with token-boundary checks
      if ( c == '&' || c == '*' ) {
        size_t j = i + 1;
        if ( j < text.size() && is_anchor_alias_name_char(text[j]) ) {
          // Scan left to previous non-whitespace character
          size_t p = i;
          while ( p > 0 && (text[p - 1] == ' ' || text[p - 1] == '\t') ) --p;
  
          bool at_value_start = ( p == 0 ) || ( text[p - 1] == '\n' )
            || is_boundary_left_char( text[p - 1] );

          // The current '&' or '*' is embedded in a scalar -> ignore
          if ( !at_value_start ) continue;
  
          // Consume anchor/alias name
          size_t k = j + 1;
          while ( k < text.size() && is_anchor_alias_name_char(text[k]) ) ++k;
  
          // Scan right to next non-whitespace (skip space, tab, CR)
          size_t r = k;
          while ( r < text.size()
            && (text[r] == ' ' || text[r] == '\t' || text[r] == '\r') ) ++r;
  
          // Token must end at a valid right boundary
          bool ends_here = ( r >= text.size() ) || ( text[r] == '\n' )
            || is_boundary_right_char( text[r] );

          if ( ends_here ) {
            std::string name = text.substr( j, k - j );
            std::ostringstream oss;
            oss << "YAML " << ( c == '&' ? "anchors" : "aliases" )
              << " are not allowed by the grammar (found '" << c << name
              << "' at line " << line << ", column " << col << ").";
            throw std::runtime_error( oss.str() );
          }
          // Otherwise ignore because the token continues (and must therefore
          // be embedded in a scalar value)
        }
      }
    }
  }
  
  // DOM-level detection of anchors/aliases, just in case
  inline void detect_anchors_or_throw( const ordered_node& node,
    const std::vector< std::string >& path )
  {
    const std::string here = join_path( path );

    if ( node.is_anchor() || node.is_alias() ) {
      std::ostringstream oss;
      oss << here << ": YAML ";
      oss << ( node.is_anchor() ? "anchors" : "aliases" );
      oss << " are not allowed by the grammar";
      throw std::runtime_error( oss.str() );
    }
  
    if ( node.is_mapping() ) {
      for ( const auto& [mk, mv] : node.map_items() ) {
        std::vector< std::string > p2 = path;
        p2.push_back( mk.get_value< std::string >() );
        detect_anchors_or_throw( mv, p2 );
      }
    }
    else if ( node.is_sequence() ) {
      for ( size_t i = 0; i < node.size(); ++i ) {
        std::vector< std::string > p2 = path;
        p2.back() = seq_indexed( p2.back(), i );
        detect_anchors_or_throw( node.at(i), p2 );
      }
    }
  }
  
  // In YODEL, a mapping is considered a "section container" if
  // - It defines template parameters, OR
  // - Any immediate child is a mapping or a sequence
  inline bool mapping_is_section_container( const ordered_node& m ) {
    if ( !m.is_mapping() ) return false;
    if ( m.contains(TEMPLATE_PARAMETERS) ) return true;
    for ( const auto& [ck, cv] : m.map_items() ) {
      if ( cv.is_mapping() || cv.is_sequence() ) return true;
    }
    return false;
  }

  // Early literal identity for sequence elements: ID -> AUTO_ID -> LOCALS.ID 
  // (string-only; no binding). This is used only for section-level overrides
  // pre-expansion where we need to target by id without performing
  // Concretize-time binding.
  inline std::optional< std::string > early_literal_element_identity_token(
    const ordered_node& el )
  {
    if ( !el.is_mapping() ) return std::nullopt;
    if ( el.contains(ID) && el.at(ID).is_string() ) {
      return to_native_checked< std::string >( el.at(ID) );
    }
    if ( el.contains(AUTO_ID) && el.at(AUTO_ID).is_string() ) {
      return to_native_checked< std::string >( el.at(AUTO_ID) );
    }
    if ( el.contains(LOCALS) && el.at(LOCALS).is_mapping() &&
      el.at(LOCALS).contains(ID) && el.at(LOCALS).at(ID).is_string() )
    {
      // locals.id must be literal here (no placeholders); we treat
      // it as-is for section overrides.
      return to_native_checked< std::string >( el.at(LOCALS).at(ID) );
    }
    return std::nullopt;
  }

  // Provenance helper used by the Concretize step.
  // If a field was cloned from base, it will appear in _base_keys. When a
  // write overrides that field, we should remove the key from _base_keys.
  inline void remove_from_base_keys_if_present( ordered_node& obj,
    const std::string& key )
  {
    if ( !obj.is_mapping() || !obj.contains(BASE_KEYS) ) return;
    const ordered_node& bk = obj.at( BASE_KEYS );
    if ( !bk.is_sequence() ) return;
  
    std::vector< ordered_node > kept;
    kept.reserve( bk.size() );
    for ( size_t i = 0; i < bk.size(); ++i ) {
      const ordered_node& nameNode = bk.at( i );
      if ( nameNode.is_string()
        && to_native_checked< std::string >(nameNode) == key ) continue;
      kept.push_back( nameNode );
    }
    obj[ BASE_KEYS ] = make_node_from( kept );
  }
  
  // Utility: is a given key listed for pruning in _locals_keys?
  inline bool is_key_listed_in_locals_keys( const ordered_node& node,
    const std::string& key )
  {
    if ( !node.contains(LOCALS_KEYS) ) return false;
    const ordered_node& lk = node.at( LOCALS_KEYS );
    if ( !lk.is_sequence() ) return false;
    for ( size_t i = 0; i < lk.size(); ++i ) {
      const ordered_node& nameNode = lk.at( i );
      if ( nameNode.is_string()
        && to_native_checked< std::string >( nameNode ) == key ) return true;
    }
    return false;
  }
  
  // Simple replace-all utility for substrings
  inline void replace_all( std::string& s, const std::string& from,
    const std::string& to )
  {
    if ( from.empty() ) return;
    std::string::size_type pos = 0;
    while ( (pos = s.find( from, pos )) != std::string::npos ) {
      s.replace( pos, from.size(), to );
      pos += to.size();
    }
  }
  
  // Bind placeholders {name} using a context map.
  // Protect doubled placeholder delimiters OPEN_PLACEHOLDER
  // and CLOSE_PLACEHOLDER and interpret them as literal single versions
  inline std::string protected_bind( const std::string& s,
    const std::unordered_map< std::string, std::string >& ctx )
  {
    static const std::string OPEN_PROTECTOR = "\x01";
    static const std::string CLOSE_PROTECTOR = "\x02";
    std::string t = s;
    replace_all( t, OPEN_PLACEHOLDER + OPEN_PLACEHOLDER, OPEN_PROTECTOR );
    replace_all( t, CLOSE_PLACEHOLDER + CLOSE_PLACEHOLDER, CLOSE_PROTECTOR );

    // Replace every placeholder key with its value. Escaped placeholder
    // delimiters have already been protected above.
    for (const auto& kv : ctx) {
      const std::string ph = OPEN_PLACEHOLDER + kv.first + CLOSE_PLACEHOLDER;
      replace_all( t, ph, kv.second );
    }
  
    replace_all( t, OPEN_PROTECTOR, OPEN_PLACEHOLDER );
    replace_all( t, CLOSE_PROTECTOR, CLOSE_PLACEHOLDER );
    return t;
  }

  // Recursively bind strings in a node using the provided context (used in the
  // Concretize step)
  inline void bind_strings_recursive( ordered_node& n,
    const std::unordered_map< std::string, std::string >& ctx )
  {
    if ( n.is_string() ) {
      const std::string s = to_native_checked<std::string>(n);
      if ( s.find(OPEN_PLACEHOLDER) != std::string::npos ||
        s.find(CLOSE_PLACEHOLDER + CLOSE_PLACEHOLDER) != std::string::npos )
      {
        n = make_node_from( protected_bind(s, ctx) );
      }
      return;
    }
    if ( n.is_mapping() ) {
      for ( const auto& [mk, mv] : n.map_items() ) {
        const std::string key = mk.get_value< std::string >();
        ordered_node child = mv;
        bind_strings_recursive( child, ctx );
        n[ key ] = child;
      }
      return;
    }
    if ( n.is_sequence() ) {
      std::vector< ordered_node > out;
      out.reserve( n.size() );
      for ( size_t i = 0; i < n.size(); ++i ) {
        ordered_node elem = n.at( i );
        bind_strings_recursive( elem, ctx );
        out.push_back( elem );
      }
      n = make_node_from( out );
      return;
    }
    // scalars/null: nothing to do
  }
  
  // Extract authored inline (non-meta) fields from a template mapping
  inline std::vector< std::pair< std::string, ordered_node > >
    collect_inline_fields_excluding_meta( const ordered_node& tmpl )
  {
    std::vector< std::pair< std::string, ordered_node > > out;
    if ( !tmpl.is_mapping() ) return out;
  
    // Meta keys to exclude when collecting inline defaults
    std::unordered_set< std::string > meta = {
      TEMPLATE_PARAMETERS, BASE, OVERRIDES, INSTANCE_FIELDS,
      OV_PER_INSTANCE, LOCALS, SWAPS
    };

    // Also exclude parameter arrays (names listed under template parameters)
    if ( tmpl.contains(TEMPLATE_PARAMETERS) ) {
      const ordered_node& plist = tmpl.at( TEMPLATE_PARAMETERS );
      for ( const auto& p_name_node : plist ) {
        if ( p_name_node.is_string() ) {
          meta.insert( to_native_checked< std::string >(p_name_node) );
        }
      }
    }
  
    for ( const auto& [mk, mv] : tmpl.map_items() ) {
      const std::string k = mk.get_value<std::string>();
      // Skip entries with meta keys
      if ( meta.count(k) ) continue;
      out.emplace_back( k, mv );
    }
    return out;
  }
  
  // Build per-index parameter contexts {name -> value_string}
  inline std::vector< std::unordered_map< std::string, std::string > >
    build_param_contexts_require( const ordered_node& tmpl,
      std::string* error_msg_out /* optional */ )
  {
    std::vector< std::unordered_map< std::string, std::string > > ctxs;
  
    if ( !tmpl.is_mapping() || !tmpl.contains(TEMPLATE_PARAMETERS) ) {
      if ( error_msg_out ) *error_msg_out = "Template entry missing required '"
        + TEMPLATE_PARAMETERS + "'.";
      return ctxs;
    }
  
    const ordered_node& p = tmpl.at( TEMPLATE_PARAMETERS );
    std::vector< std::string > params;
    for ( const auto& pname : p ) {
      params.push_back( to_native_checked< std::string >(pname) );
    }
    if ( params.empty() ) {
      ctxs.push_back( {} ); // single empty context
      return ctxs;
    }
  
    // Determine length N from the first parameter array
    const size_t N = tmpl.at( params[0] ).size();
    for ( size_t j = 1; j < params.size(); ++j ) {
      if ( tmpl.at(params[j]).size() != N ) {
        if ( error_msg_out ) {
          std::ostringstream oss;
          oss << "Template parameter lists have unequal lengths: ";
          for ( const auto& pp : params ) oss << pp
            << "=" << tmpl.at( pp ).size() << " ";
          *error_msg_out = oss.str();
        }
        return {};
      }
    }
  
    ctxs.reserve( N );
    for ( size_t i = 0; i < N; ++i ) {
      std::unordered_map< std::string, std::string > ctx;
      for ( const auto& pkey : params ) {
        ctx[ pkey ] = to_string_any( tmpl.at(pkey).at(i) );
      }
      ctxs.push_back( std::move(ctx) );
    }
    return ctxs;
  }
  
  // Detect unresolved true placeholders like "{name}" after a binding pass
  inline bool contains_true_placeholder( const std::string& s ) {
    static const std::regex ph( R"(\)" + OPEN_PLACEHOLDER
    + R"("[A-Za-z_][A-Za-z0-9_]*\)" + CLOSE_PLACEHOLDER );
    return std::regex_search( s, ph );
  }
  
  inline ordered_node evaluate_conditional_per_instance(
    // sequence of { when, replace }
    const ordered_node& rules,
    // Concretize context (parameters + locals)
    const std::unordered_map< std::string, std::string >& pctx )
  {
    ordered_node overlay = ordered_node::mapping();
    if ( !rules.is_sequence() ) return overlay; // empty
  
    // Accumulate replacements
    auto key_equals = [&]( const std::string& key,
      const ordered_node& criterion ) -> bool
    {
      auto it = pctx.find( key );
      if ( it == pctx.end() ) return false;
      std::string actual = it->second;
  
      if ( criterion.is_sequence() ) {
        // OR semantics: match if any element equals
        for ( size_t i = 0; i < criterion.size(); ++i ) {
          if ( actual == to_string_any(criterion.at( i )) ) return true;
        }
        return false;
      }
      // scalar match
      return actual == to_string_any( criterion );
    };
  
    for ( size_t ri = 0; ri < rules.size(); ++ri ) {
      ordered_node rule = rules.at( ri );
      if ( !rule.is_mapping() || !rule.contains(WHEN)
        || !rule.contains(REPLACE) ) continue;

      ordered_node when = rule.at( WHEN );
      ordered_node repl = rule.at( REPLACE );
  
      // All keys in 'when' must match; each key supports OR
      // semantics via sequence
      bool match = true;
      if ( when.is_mapping() ) {
        for ( const auto& [wk, wv] : when.map_items() ) {
          const std::string key = wk.get_value< std::string >();
          if ( !key_equals(key, wv) ) { match = false; break; }
        }
      }
      else {
        match = false;
      }
      if ( !match ) continue;
  
      // Last match wins: deep-merge overlay with repl (repl wins on conflicts)
      overlay = deep_merge( overlay, repl );
    }
    return overlay;
  }
  
  // Build a root-level context frame from the DOM:
  // - root.locals scalars and strings
  // - other root-level simple scalars/strings
  //   (excluding meta + section containers)
  inline std::unordered_map< std::string, std::string >
    collect_root_context( const ordered_node& doc )
  {
    std::unordered_map< std::string, std::string > ctx;
    if ( !doc.is_mapping() ) return ctx;
  
    // 1) root.locals first (locals override other root fields)
    if ( doc.contains(LOCALS) ) {
      const ordered_node& loc = doc.at( LOCALS );
      if ( loc.is_mapping() ) {
        for ( const auto& [mk, mv] : loc.map_items() ) {
          const std::string k = mk.get_value< std::string >();
          if ( is_non_null_scalar(mv) )
          {
            ctx[ k ] = to_string_any( mv );
          }
        }
      }
    }
  
    // 2) other root-level simple strings/scalars
    // (skip meta and section containers)
    for ( const auto& [mk, mv] : doc.map_items() ) {
      const std::string k = mk.get_value< std::string >();
      if ( k == OVERRIDES || k == SWAPS || k == LOCALS ) continue;
      if ( mv.is_mapping() && mapping_is_section_container(mv) ) continue;
      if ( mv.is_sequence() ) continue;
  
      if ( is_non_null_scalar(mv) )
      {
        if ( !ctx.count(k) ) ctx[ k ] = to_string_any( mv );
      }
    }
  
    return ctx;
  }

} // namespace yodel::internal

} // namespace yodel

// IdIndex member function definitions
inline std::string yodel::internal::IdIndex::parent_of(
  const std::string& scope )
{
  if (scope.empty()) return std::string();
  std::size_t pos = scope.rfind( PATH_DELIMITER );
  if ( pos == std::string::npos ) return std::string(); // ROOT has no parent
  return scope.substr( 0, pos );
}

inline void yodel::internal::IdIndex::register_child_scope(
  const std::string& parent_scope, const std::string& child_scope )
{
  children[ parent_scope ].push_back( child_scope );
}

inline void yodel::internal::IdIndex::register_token( const std::string& scope,
  const std::string& token, const ordered_node& obj )
{
  auto& bucket = table[ scope ];
  if ( bucket.count(token) ) {
    std::ostringstream oss;
    oss << "Duplicate token '" << token << "' within scope '" << scope << "'";
    throw std::runtime_error( oss.str() );
  }
  const std::string canon = scope.empty() ? token : ( scope + "." + token );
  bucket[ token ] = canon;
  canonical_nodes[ canon ] = obj;
}

inline bool yodel::internal::IdIndex::has_canonical(
  const std::string& canon ) const
{
  return canonical_nodes.count( canon ) > 0;
}

inline std::optional< std::string > yodel::internal
  ::IdIndex::resolve_qualified( const std::string& token ) const
{
  if ( token.rfind(DOC_ROOT + PATH_DELIMITER, 0) != 0 ) return std::nullopt;
  if ( has_canonical(token) ) return token;
  return std::nullopt;
}

// Unqualified tokens: single symbol, sibling-first ladder with rung-local ambiguity
inline std::string yodel::internal::IdIndex::resolve_via_ladder(
  const std::string& start_scope, const std::string& token ) const
{
  auto find_in_bucket = [&]( const std::string& scope )
    -> std::optional< std::string >
  {
    auto it = table.find( scope );
    if ( it == table.end() ) return std::nullopt;
    auto it2 = it->second.find( token );
    if ( it2 == it->second.end() ) return std::nullopt;
    return it2->second; // canonical identity
  };

  auto collect_hits = [&]( const std::vector< std::string >& scopes )
    -> std::vector<std::string>
  {
    std::vector< std::string > out;
    for ( const auto& s : scopes ) {
      if ( auto h = find_in_bucket(s) ) out.push_back( *h );
    }
    return out;
  };

  std::string cur = start_scope;
  while ( true ) {
    // Rung 1: current scope
    if ( auto h = find_in_bucket(cur) ) return *h;

    // Rung 2: current scope’s siblings
    std::string p = parent_of( cur );
    if ( !p.empty() ) {
      std::vector< std::string > sibs;
      auto it = children.find( p );
      if ( it != children.end() ) {
        for ( const auto& child : it->second ) {
          if ( child != cur ) sibs.push_back( child );
        }
      }
      auto hits = collect_hits( sibs );
      if ( hits.size() == 1 ) return hits.front();
      if ( hits.size() > 1 ) {
        std::ostringstream oss;
        oss << "Ambiguous unqualified reference '" << token
          << "' at siblings of scope '" << cur << "': ";
        for ( size_t i = 0; i < hits.size(); ++i ) {
          if ( i ) oss << ", ";
          oss << hits[ i ];
        }
        throw std::runtime_error( oss.str() );
      }
    }

    // Rung 3: parent scope
    if ( !p.empty() ) {
      if ( auto h = find_in_bucket(p) ) return *h;
    }

    // Rung 4: parent’s siblings
    std::string pp = parent_of( p );
    if ( !pp.empty() ) {
      std::vector< std::string > psibs;
      auto it = children.find( pp );
      if ( it != children.end() ) {
        for ( const auto& child : it->second ) {
          if ( child != p ) psibs.push_back( child );
        }
      }
      auto hits = collect_hits( psibs );
      if ( hits.size() == 1 ) return hits.front();
      if ( hits.size() > 1 ) {
        std::ostringstream oss;
        oss << "Ambiguous unqualified reference '" << token
          << "' at siblings of scope '" << p << "': ";
        for ( std::size_t i = 0; i < hits.size(); ++i ) {
          if ( i ) oss << ", ";
          oss << hits[ i ];
        }
        throw std::runtime_error( oss.str() );
      }
    }

    // Climb
    if ( cur.empty() || p.empty() ) break;
    cur = p;
  }

  std::ostringstream oss;
  oss << "Unqualified reference '" << token
    << "' not found via ladder for scope '" << start_scope << "'.";
  throw std::runtime_error( oss.str() );
}

// Attempt a complete chain "[s1, s2, ...]" starting from base scope.
// Returns the canonical identity of the last segment if the full chain exists.
// Otherwise returns std::nullopt.
inline std::optional< std::string > yodel::internal::IdIndex::try_chain_once(
  const std::string& base_scope, const std::vector< std::string >& segs ) const
{
  if ( segs.empty() ) return std::nullopt;

  // Step 1: s1 under base_scope
  auto it_base = table.find( base_scope );
  if ( it_base == table.end() ) return std::nullopt;
  auto it_s1 = it_base->second.find( segs[0] );
  if ( it_s1 == it_base->second.end() ) return std::nullopt;
  std::string cur_scope = it_s1->second; // canonical scope after s1

  // Steps 2..n: under cur_scope
  for ( std::size_t i = 1; i < segs.size(); ++i ) {
    auto it_cur = table.find( cur_scope );
    if ( it_cur == table.end() ) return std::nullopt;
    auto it_si = it_cur->second.find( segs[i] );
    if ( it_si == it_cur->second.end() ) return std::nullopt;
    cur_scope = it_si->second; // advance
  }

  // cur_scope is canonical identity of the final segment
  return cur_scope;
}

// Determine canonical identity for partially-scoped tokens, i.e., those
// that do not start with DOC_ROOT + PATH_DELIMITER but contain at least
// one instance of PATH_DELIMITER
inline std::string yodel::internal::IdIndex::resolve_partial_chain(
  const std::string& start_scope,
  const std::vector< std::string >& segs ) const
{
  auto collect_chain_hits = [&]( const std::vector< std::string >& bases )
    -> std::vector< std::string >
  {
    std::vector< std::string > out;
    for ( const auto& s : bases ) {
      if ( auto h = try_chain_once(s, segs) ) out.push_back( *h );
    }
    return out;
  };

  std::string cur = start_scope;
  while ( true ) {
    // Rung 1: current scope
    if ( auto h = try_chain_once(cur, segs) ) return *h;

    // Rung 2: current scope’s siblings
    std::string p = parent_of( cur );
    if ( !p.empty() ) {
      std::vector< std::string > sibs;
      auto it = children.find( p );
      if ( it != children.end() ) {
        for ( const auto& child : it->second ) {
          if ( child != cur ) sibs.push_back( child );
        }
      }
      auto hits = collect_chain_hits( sibs );
      if ( hits.size() == 1 ) return hits.front();
      if ( hits.size() > 1 ) {
        std::ostringstream oss;
        oss << "Ambiguous partial reference '";
        for ( size_t i = 0; i < segs.size(); ++i ) {
          if ( i ) oss << PATH_DELIMITER;
          oss << segs[ i ];
        }
        oss << "' at siblings of scope '" << cur << "': ";
        for ( std::size_t i = 0; i < hits.size(); ++i ) {
          if ( i ) oss << ", ";
          oss << hits[ i ];
        }
        throw std::runtime_error( oss.str() );
      }
    }

    // Rung 3: parent scope
    if ( !p.empty() ) {
      if ( auto h = try_chain_once(p, segs) ) return *h;
    }

    // Rung 4: parent’s siblings
    std::string pp = parent_of( p );
    if ( !pp.empty() ) {
      std::vector< std::string > psibs;
      auto it = children.find( pp );
      if ( it != children.end() ) {
        for ( const auto& child : it->second ) {
          if ( child != p ) psibs.push_back( child );
        }
      }
      auto hits = collect_chain_hits( psibs );
      if ( hits.size() == 1 ) return hits.front();
      if ( hits.size() > 1 ) {
        std::ostringstream oss;
        oss << "Ambiguous partial reference '";
        for ( size_t i = 0; i < segs.size(); ++i ) {
          if ( i ) oss << PATH_DELIMITER;
          oss << segs[ i ];
        }
        oss << "' at siblings of scope '" << p << "': ";
        for ( size_t i = 0; i < hits.size(); ++i ) {
          if ( i ) oss << ", ";
          oss << hits[ i ];
        }
        throw std::runtime_error( oss.str() );
      }
    }

    // Climb
    if ( cur.empty() || p.empty() ) break;
    cur = p;
  }

  std::ostringstream oss;
  oss << "Partial reference '";
  for (std::size_t i = 0; i < segs.size(); ++i) {
    if (i) oss << PATH_DELIMITER;
    oss << segs[ i ];
  }
  oss << "' not found via ladder for scope '" << start_scope << "'.";
  throw std::runtime_error( oss.str() );
}

// Resolver member function definitions

inline bool yodel::Resolver::ResolveSession
  ::OverridesForInstance::has_any() const
{
  return ( per_instance_map.is_mapping() && per_instance_map.size() > 0) ||
    ( per_instance_seq.is_sequence() && per_instance_seq.size() > 0) ||
    ( broadcast_map.is_mapping() && broadcast_map.size() > 0) ||
    ( conditional_overlay.is_mapping() && conditional_overlay.size() > 0 );
}

// Read from an input stream until end-of-file, then apply full processing
// on the resulting string
inline yodel::ordered_node yodel::Resolver::resolve( std::istream& in ) {
  std::ostringstream ss;
  ss << in.rdbuf();
  return this->resolve( ss.str() );
}

// Main implementation of YODEL -> plain YAML resolution
inline yodel::ordered_node
  yodel::Resolver::resolve( const std::string& text )
{
  // Rebuild default session state for this call
  session_ = ResolveSession();

  // Preflight anchors/aliases check + parse + DOM anchors/aliases check
  doc_ = this->parse_and_preflight( text );

  // Initialize path/scope
  session_.path_stack.clear();
  session_.path_stack.push_back( internal::DOC_ROOT );
  session_.scope_path = internal::DOC_ROOT;

  // 1) Section-level overrides (pre-expansion)
  this->apply_section_overrides_in_place();

  // 2) Collect identities & Concretize (template expansion, provenance-gated
  // defaults)
  this->collect_index_and_concretize();

  // 3) Resolve (overrides, base merges, swaps, binding;
  // update canonical identities)
  this->resolve_unified();

  // 4) Final prune
  this->prune_final();

  return doc_;
}

inline yodel::ordered_node yodel::Resolver::parse_and_preflight(
  const std::string& yaml_text )
{
  // 1) Raw-text preflight: reject anchors/aliases outside quotes/comments
  internal::preflight_reject_anchors_aliases( yaml_text );

  // 2) Parse into DOM
  ordered_node dom = ordered_node::deserialize( yaml_text );

  // 3) DOM-level anchors/aliases rejection
  std::vector<std::string> root_path = { "root" };
  internal::detect_anchors_or_throw( dom, root_path );

  return dom;
}

[[noreturn]] inline void yodel::Resolver::throw_error_at(
  const std::string& msg,
  const std::optional< std::string >& hint )
{
  // Compose "root.a.b[0].c: message (hint)" with canonical scope in brackets
  std::ostringstream oss;
  const std::string path = internal::join_path( session_.path_stack );
  oss << path;
  if ( !session_.scope_path.empty() ) {
    oss << " [" << session_.scope_path << ']';
  }
  oss << ": " << msg;
  if ( hint && !hint->empty() ) {
    oss << " (" << *hint << ')';
  }
  throw std::runtime_error( oss.str() );
}

// Manages context stack for the Resolve step.
// Push scalar/string fields from a mapping into the top-of-stack context.
// Nearest-wins: child writes shadow ancestor entries while this frame is
// active.
inline void yodel::Resolver::ctx_push( const ordered_node& mapping_fields ) {
  std::unordered_map< std::string, std::string > frame;
  if ( mapping_fields.is_mapping() ) {
    for ( const auto& [mk, mv] : mapping_fields.map_items() ) {
      const std::string key = mk.get_value< std::string >();
      if ( internal::is_non_null_scalar(mv) ) {
        frame[ key ] = internal::to_string_any( mv );
      }
    }
  }
  session_.ctx_stack.push_back( std::move(frame) );
}

// Pop the most-recent context frame.
inline void yodel::Resolver::ctx_pop() {
  if ( !session_.ctx_stack.empty() ) {
    session_.ctx_stack.pop_back();
  }
}

inline void yodel::Resolver::apply_section_overrides_in_place() {
  using internal::OVERRIDES;
  if ( !doc_.is_mapping() || !doc_.contains(OVERRIDES) ) return;
  const ordered_node ov = doc_.at( OVERRIDES );
  if ( !ov.is_mapping() ) return;

  for ( const auto& [mk, mv] : ov.map_items() ) {
    const std::string section = mk.get_value< std::string >();
    if ( !doc_.contains(section) ) continue; // silently skip unknown sections

    // Maintain path stack for error reporting. Note that we record
    // the path stack size before pushing here.
    const std::size_t parent_guard = session_.path_stack.size();
    session_.path_stack.push_back( section );

    ordered_node sec = doc_.at( section );

    // Sequence-by-id form: target is a sequence and override value is a mapping
    if ( sec.is_sequence() && mv.is_mapping() ) {

      // Build overlay_by_id and set of required ids (single string keys only)
      std::unordered_map< std::string, ordered_node > overlay_by_id;
      std::unordered_set< std::string > required_ids;

      for ( const auto& [ok, ovObj] : mv.map_items() ) {
        const std::string id = ok.get_value< std::string >();
        overlay_by_id.emplace( id, ovObj );
        required_ids.insert( id );
      }

      // Merge into sequence elements by id; track which ids were resolved
      std::unordered_set< std::string > resolved_ids;
      std::vector< ordered_node > new_seq;
      new_seq.reserve( sec.size() );

      for ( std::size_t i = 0; i < sec.size(); ++i ) {
        ordered_node el = sec.at( i );
        if ( auto eid_opt
          = internal::early_literal_element_identity_token(el) )
        {
          const std::string& eid = *eid_opt;
          auto it = overlay_by_id.find( eid );
          if ( it != overlay_by_id.end() ) {
            el = internal::deep_merge( el, it->second );
            resolved_ids.insert( eid );
          }
        }
        new_seq.push_back( el );
      }

      // Strict unknown ids: any authored id not found in the sequence
      for ( const auto& need : required_ids ) {
        if ( !resolved_ids.count(need) ) {
          // Collect available ids for diagnostics
          std::vector< std::string > available;
          for ( size_t si = 0; si < sec.size(); ++si ) {
            const ordered_node el2 = sec.at( si );
            if ( auto eid
              = internal::early_literal_element_identity_token(el2) )
            {
              available.push_back( *eid );
            }
          }
          std::ostringstream msg;
          msg << "Section-level override targets unknown id '" << need
            << "' for section '" << section << "'";
          if ( !available.empty() ) {
            msg << "; available: ";
            for ( size_t i = 0; i < available.size(); ++i ) {
              if ( i ) msg << ", ";
              msg << available[ i ];
            }
          }
          throw_error_at( msg.str() );
        }
      }

      // Write back; order preserved
      doc_[section] = internal::make_node_from( new_seq );

      // Restore path stack
      while ( session_.path_stack.size() > parent_guard ) {
        session_.path_stack.pop_back();
      }
      continue;
    }

    // Mapping section: deep-merge keys; scalars/sequences replace; null clears
    if ( sec.is_mapping() && mv.is_mapping() ) {
      ordered_node new_sec = ordered_node::mapping();
      for ( const auto& [ck, cv] : sec.map_items() ) {
        const std::string childKey = ck.get_value< std::string >();
        ordered_node childVal = cv;
        if ( mv.contains(childKey) ) {
          const ordered_node ovChild = mv.at( childKey );
          if ( ovChild.is_mapping() && childVal.is_mapping() ) {
            childVal = internal::deep_merge( childVal, ovChild );
          }
          else {
            // scalars/sequences replace; null clears
            childVal = ovChild;
          }
        }
        new_sec[childKey] = childVal;
      }
      doc_[section] = new_sec;

      while ( session_.path_stack.size() > parent_guard ) {
        session_.path_stack.pop_back();
      }
      continue;
    }

    doc_[section] = mv;

    while ( session_.path_stack.size() > parent_guard ) {
      session_.path_stack.pop_back();
    }
  }
}

// Single helper used to handle VALUE_FROM in both the Concretize and Resolve
// steps. Errors are handled with throw_error_at() and include path/scope from
// session_.
inline yodel::ordered_node yodel::Resolver::materialize_value_from(
  const ordered_node& authored,
  const std::unordered_map< std::string, std::string >& bind_ctx,
  const char* usage_label, std::optional< std::string > required_shape,
  BinderMode mode )
{
  if ( !authored.is_mapping() || !authored.contains(internal::VALUE_FROM) )
    return authored;

  const ordered_node tokNode = authored.at( internal::VALUE_FROM );
  if ( !tokNode.is_string() ) {
    std::ostringstream oss;
    oss << ( mode == BinderMode::Concretize ? "Concretize: " : "Resolve: " )
      << "value from for " << usage_label << " must be a string token.";
    throw_error_at( oss.str() );
  }

  // Bind the token with the provided context (params/local in Concretize;
  // inherited in Resolve)
  std::string tok = internal::protected_bind(
    internal::to_native_checked< std::string >(tokNode), bind_ctx );

  // Resolve token to canonical identity via IdIndex
  std::string canon;
  const internal::TokenKind kind = internal::classify_token( tok );
  try {
    if ( kind == internal::TokenKind::Qualified ) {
      auto cq = session_.ids.resolve_qualified( tok );
      if ( !cq ) {
        std::ostringstream oss;
        oss << ( mode == BinderMode::Concretize ? "Concretize: " : "Resolve: " )
          << "value from could not resolve qualified token '" << tok << "'";
        throw_error_at( oss.str(),
          std::optional< std::string >( "resolution: qualified" )
        );
      }
      canon = *cq;
    }
    else if ( kind == internal::TokenKind::Partial ) {
      auto segs = internal::split_segments( tok );
      canon = session_.ids.resolve_partial_chain( session_.scope_path, segs );
    }
    else {
      canon = session_.ids.resolve_via_ladder( session_.scope_path, tok );
    }
  }
  catch ( const std::exception& ) {
    std::ostringstream oss;
    oss << ( mode == BinderMode::Concretize ? "Concretize: " : "Resolve: " )
      << "value from could not resolve token '" << tok << "'";
    const char* hint = ( kind == internal::TokenKind::Qualified
      ? "resolution: qualified" : "resolution: ladder" );
    throw_error_at( oss.str(), std::optional< std::string >(hint) );
  }

  ordered_node imported = session_.ids.canonical_nodes.at( canon );

  // Shape enforcement (if requested)
  if ( required_shape ) {
    const std::string shape = *required_shape;
    const bool is_scalar = internal::is_non_null_scalar( imported );
    const std::string got = ( imported.is_mapping() ? "mapping" :
      ( imported.is_sequence() ? "sequence" :
        ( imported.is_null() ? "null" :
          (is_scalar ? "scalar" : "unknown" )
        )
      )
    );

    if ( shape == "sequence" && !imported.is_sequence() ) {
      std::ostringstream oss;
      oss << ( mode == BinderMode::Concretize ? "Concretize: " : "Resolve: " )
        << "value from for " << usage_label
        << " must resolve to a sequence (got: " << got << ")";
      throw_error_at( oss.str() );
    }
    if ( shape == "scalar" && !is_scalar ) {
      std::ostringstream oss;
      oss << ( mode == BinderMode::Concretize ? "Concretize: " : "Resolve: " )
        << "value from for " << usage_label
        << " must resolve to a scalar (got: " << got << ")";
      throw_error_at( oss.str() );
    }
    if ( shape == "mapping" && !imported.is_mapping() ) {
      std::ostringstream oss;
      oss << ( mode == BinderMode::Concretize ? "Concretize: " : "Resolve: " )
        << "value from for " << usage_label
        << " must resolve to a mapping (got: " << got << ")";
      throw_error_at( oss.str() );
    }
  }

  // If authored has extra keys beyond `value from` and imported is a mapping,
  // deep-merge (authored wins)
  if ( imported.is_mapping() ) {
    ordered_node overlay = ordered_node::mapping();
    for ( const auto& [mk, mv] : authored.map_items() ) {
      const std::string k = mk.get_value< std::string >();
      if ( k == internal::VALUE_FROM ) continue;
      overlay[ k ] = mv;
    }
    if ( !overlay.is_null() ) {
      imported = internal::deep_merge( imported, overlay );
    }
  }

  return imported;
}

// Constructs the local context for the Concretize step
inline std::unordered_map< std::string, std::string >
  yodel::Resolver::build_concretize_local_ctx(
    const ordered_node& obj_or_template,
    const std::unordered_map<std::string, std::string>& params )
{
  // Start with the input parameters (from the template definition)
  std::unordered_map< std::string, std::string > local_ctx = params;

  // If there are authored locals, bind in author order with intra-locals
  // chaining
  if ( obj_or_template.is_mapping()
    && obj_or_template.contains(internal::LOCALS)
    && obj_or_template.at(internal::LOCALS).is_mapping() )
  {
    const ordered_node authored = obj_or_template.at( internal::LOCALS );
    ordered_node bound_locals = ordered_node::mapping();

    // Chain map for locals (inside locals only)
    std::unordered_map< std::string, std::string > chain_ctx = local_ctx;

    for ( const auto& [lk, lv] : authored.map_items() ) {
      const std::string name = lk.get_value< std::string >();
      // First, materialize "value from" if present
      ordered_node v2 = materialize_value_from( lv, local_ctx,
        ( internal::LOCALS + internal::PATH_DELIMITER + name ).c_str(),
        std::nullopt /*shape*/, BinderMode::Concretize );
      if ( v2.is_string() ) {
        const std::string bound = internal::protected_bind(
          internal::to_native_checked< std::string >( v2 ), chain_ctx );
        bound_locals[ name ] = internal::make_node_from( bound );
        chain_ctx[ name ] = bound; // allow chaining
        local_ctx[ name ] = bound; // contribute bare name to ctx
      }
      else {
        bound_locals[ name ] = v2;
        // contribute simple scalars to ctx
        if ( internal::is_non_null_scalar(bound_locals[ name ]) )
        {
          local_ctx[ name ] = internal::to_string_any( bound_locals[name] );
        }
      }
    }
  }

  return local_ctx;
}

// Performs actual template expansion and Concretize writes
inline void yodel::Resolver::collect_index_and_concretize() {

  using internal::AUTO_ID;
  using internal::BASE;
  using internal::BASE_KEYS;
  using internal::ID;
  using internal::INSTANCE_FIELDS;
  using internal::LITERAL_SWAPS;
  using internal::LOCALS;
  using internal::LOCALS_KEYS;
  using internal::OVERRIDES;
  using internal::OV_PER_INSTANCE;
  using internal::PATH_DELIMITER;
  using internal::SWAPS;
  using internal::TEMPLATE_PARAMETERS;

  // Recursive Concretize walker: expands templates wherever they appear
  struct Walker {
    Resolver* self;

    // Expand mapping template to sequence of concretized instances
    std::vector< ordered_node > expand_template_mapping( ordered_node tmpl,
      const std::string& section_scope,
      const std::string& path_key_for_errors )
    {
      // Pre-materialize parameter arrays to sequences (no per-index params yet)
      if ( tmpl.contains(TEMPLATE_PARAMETERS) ) {
        const ordered_node& paramsList = tmpl.at( TEMPLATE_PARAMETERS );
        for ( const auto& pnameNode : paramsList ) {
          const std::string pname
            = internal::to_native_checked< std::string >( pnameNode );
          if ( !tmpl.contains(pname) ) {
            std::ostringstream oss;
            oss << "Concretize: missing parameter array '" << pname
              << "' (author a sequence or use value from).";
            self->throw_error_at( oss.str() );
          }
          // materialize to sequence (Concretize mode), no context needed here
          std::unordered_map< std::string, std::string > empty_ctx;
          ordered_node resolved = self->materialize_value_from(
            tmpl.at(pname), empty_ctx,
            ( std::string("parameter '") + pname + "'" ).c_str(),
            std::optional< std::string >( "sequence" ),
            BinderMode::Concretize
          );
          if ( !resolved.is_sequence() ) {
            std::ostringstream oss;
            oss << "Concretize: parameter '" << pname
              << "' must be a sequence (got non-sequence)";
            self->throw_error_at( oss.str() );
          }
          tmpl[ pname ] = resolved;
        }
      }

      // Build per-index parameter contexts (require equal-length lists)
      std::string err;
      auto ctxs = internal::build_param_contexts_require( tmpl, &err );
      if ( ctxs.empty() && !err.empty() ) {
        std::ostringstream oss;
        oss << "Concretize: " << err;
        self->throw_error_at( oss.str() );
      }
      // Number of instances to be fully concretized from this template
      const std::size_t N = ctxs.size();

      // Inline (non-meta) fields; meta keys excluded like in current code
      auto inline_fields
        = internal::collect_inline_fields_excluding_meta( tmpl );

      const bool has_base = tmpl.contains( BASE );
      if ( has_base && !tmpl.at(BASE).is_string() ) {
        self->throw_error_at( "'" + BASE
          + "' must be a string reference token." );
      }

      // Capture authored overrides (broadcast & per-instance) for later Resolve
      ordered_node overrides_broadcast = ordered_node::mapping();
      if ( tmpl.contains(OVERRIDES) && tmpl.at(OVERRIDES).is_mapping() ) {
        overrides_broadcast = tmpl.at( OVERRIDES );
      }
      ordered_node overrides_per_instance_map = ordered_node::mapping();
      ordered_node overrides_per_instance_seq = ordered_node::sequence();
      if ( tmpl.contains(OV_PER_INSTANCE) ) {
        const ordered_node& opi = tmpl.at( OV_PER_INSTANCE );
        if ( opi.is_mapping() ) overrides_per_instance_map = opi;
        else if ( opi.is_sequence() ) overrides_per_instance_seq = opi;
      }

      std::vector< ordered_node > instances;
      instances.reserve( N );

      for ( std::size_t i = 0; i < N; ++i ) {
        const auto& params_ctx = ctxs[ i ];

        // Object-level base clone (bound with parameters)
        ordered_node base_clone = ordered_node::mapping();
        if ( has_base ) {
          const std::string base_raw
            = internal::to_native_checked< std::string >( tmpl.at(BASE) );
          std::string tok = internal::protected_bind( base_raw, params_ctx );
          std::string canon;
          const internal::TokenKind kind = internal::classify_token( tok );
          try {
            if ( kind == internal::TokenKind::Qualified ) {
              auto c = self->session_.ids.resolve_qualified( tok );
              if ( !c ) throw std::runtime_error( "unknown qualified " + BASE );
              canon = *c;
            }
            else if ( kind == internal::TokenKind::Partial ) {
              canon = self->session_.ids.resolve_partial_chain( section_scope,
                internal::split_segments(tok) );
            }
            else {
              canon = self->session_.ids
                .resolve_via_ladder( section_scope, tok );
            }
          }
          catch (const std::exception&) {
            std::ostringstream oss;
            oss << "Unknown " + BASE + " '" << tok << "'";
            const char* hint = ( kind == internal::TokenKind::Qualified
              ? "resolution: qualified" : "resolution: ladder" );
            self->throw_error_at( oss.str(),
              std::optional< std::string >(hint) );
          }
          base_clone = self->session_.ids.canonical_nodes.at( canon );
        }

        // Start object and provenance
        ordered_node obj = ordered_node::mapping();
        std::unordered_set< std::string > base_keys_set;
        if ( !base_clone.is_null() ) {
          obj = base_clone;
          std::vector< ordered_node > baseKeysSeq;
          for ( const auto& [bk, bv] : base_clone.map_items() ) {
            const std::string name = bk.get_value< std::string >();
            baseKeysSeq.push_back( internal::make_node_from(name) );
            base_keys_set.insert( name );
          }
          if ( !baseKeysSeq.empty() ) obj[ BASE_KEYS ]
            = internal::make_node_from( baseKeysSeq );
        }

        // Locals binding (params/local chaining)
        std::unordered_map< std::string, std::string > local_ctx
          = self->build_concretize_local_ctx( tmpl, params_ctx );

        if ( tmpl.contains(LOCALS) && tmpl.at(LOCALS).is_mapping() ) {
          ordered_node bound_locals = ordered_node::mapping();
          const ordered_node authored = tmpl.at( LOCALS );
          std::vector< std::string > locals_keys_list;

          for ( const auto& [lk, lv] : authored.map_items() ) {
            const std::string name = lk.get_value< std::string >();
            ordered_node v2 = self->materialize_value_from(
              lv, local_ctx,
              ( LOCALS + PATH_DELIMITER + name ).c_str(),
              std::nullopt, BinderMode::Concretize );
            if ( v2.is_string() ) {
              const std::string bound = internal::protected_bind(
                internal::to_native_checked< std::string >( v2 ),
                local_ctx
              );
              bound_locals[ name ] = internal::make_node_from( bound );
              local_ctx[ name ] = bound;
            }
            else {
              bound_locals[ name ] = v2;
              if ( internal::is_non_null_scalar(bound_locals[ name ]) ) {
                local_ctx[ name ]
                  = internal::to_string_any( bound_locals[name] );
              }
            }
            locals_keys_list.push_back( name );
          }
          obj[ LOCALS ] = bound_locals;

          // Attach/merge _locals_keys (union of base + current locals)
          std::unordered_set< std::string > merged;
          if ( obj.contains(LOCALS_KEYS)
            && obj.at(LOCALS_KEYS).is_sequence() )
          {
            const ordered_node& lk = obj.at( LOCALS_KEYS );
            for ( const auto& nn : lk ) {
              if ( nn.is_string() ) {
                merged.insert( internal::to_native_checked<std::string>(nn) );
              }
            }
          }
          for ( const auto& name : locals_keys_list ) merged.insert( name );
          if ( !merged.empty() ) {
            std::vector< ordered_node > seq;
            seq.reserve( merged.size() );
            for ( const auto& name : merged ) {
              seq.push_back( internal::make_node_from(name) );
            }
            obj[ LOCALS_KEYS ] = internal::make_node_from( seq );
          }
        }

        // Instance fields (per-index)
        if ( tmpl.contains(INSTANCE_FIELDS)
          && tmpl.at(INSTANCE_FIELDS).is_mapping() )
        {
          const ordered_node inst = tmpl.at( INSTANCE_FIELDS );
          for ( const auto& [mk, mv] : inst.map_items() ) {
            const std::string k = mk.get_value< std::string >();
            ordered_node mv2 = self->materialize_value_from(
              mv, params_ctx,
              ( std::string("instance field '") + k + "'" ).c_str(),
              std::optional< std::string >( "sequence" ),
              BinderMode::Concretize
            );
            if ( !mv2.is_sequence() ) {
              std::ostringstream oss;
              oss << "Concretize: instance field '"
                << k << "' must be a sequence";
              self->throw_error_at( oss.str() );
            }
            ordered_node vi = mv2.at( i );
            ordered_node vi2 = self->materialize_value_from(
              vi, params_ctx,
              ( std::string("field '") + k + "'" ).c_str(),
              std::nullopt, BinderMode::Concretize
            );

            if ( vi2.is_string() ) {
              obj[ k ] = internal::make_node_from(
                internal::protected_bind(
                  internal::to_native_checked< std::string >( vi2 ),
                params_ctx )
              );
            }
            else if ( vi2.is_mapping() ) {
              bool contains_map_k = obj.contains( k ) && obj.at(k).is_mapping();
              ordered_node merged = contains_map_k
                ? internal::deep_merge( obj.at(k), vi2 ) : vi2;
              obj[k] = merged;
              internal::bind_strings_recursive( obj[k], params_ctx );
            }
            else if ( vi2.is_sequence() ) {
              obj[ k ] = vi2;
              internal::bind_strings_recursive( obj[k], params_ctx );
            }
            else {
              obj[ k ] = vi2;
            }
            internal::remove_from_base_keys_if_present( obj, k );

            if ( internal::is_non_null_scalar(obj[ k ])  ) {
              local_ctx[ k ] = internal::to_string_any( obj[k] );
            }
          }
        }

        // Inline defaults (provenance-gated)
        for ( const auto& kv : inline_fields ) {
          const std::string& k = kv.first;
          ordered_node v = self->materialize_value_from(
            kv.second, local_ctx,
            ( std::string("field '") + k + "'" ).c_str(),
            std::nullopt, BinderMode::Concretize
          );
          const bool exists = obj.contains( k );
          const bool is_null = exists && obj.at( k ).is_null();
          const bool from_base = base_keys_set.count( k ) > 0;
          const bool can_write = ( !exists ) || is_null || from_base;
          if ( !can_write ) continue;

          if ( v.is_string() ) {
            obj[ k ] = internal::make_node_from(
              internal::protected_bind(
                internal::to_native_checked< std::string >( v ),
                local_ctx )
            );
          }
          else if ( v.is_mapping() ) {
            if ( exists && obj.at(k).is_mapping() && from_base ) {
              obj[ k ] = internal::deep_merge( obj.at(k), v );
              internal::bind_strings_recursive( obj[k], local_ctx );
            }
            else {
              obj[ k ] = v;
              internal::bind_strings_recursive( obj[k], local_ctx );
            }
          }
          else if ( v.is_sequence() ) {
            obj[ k ] = v;
            internal::bind_strings_recursive( obj[k], local_ctx );
          }
          else {
            obj[ k ] = v;
          }

          if ( internal::is_non_null_scalar(obj[ k ]) ) {
            local_ctx[ k ] = internal::to_string_any( obj[k] );
          }
        }

        // Swaps: bind to literals and attach _literal_swaps
        if ( tmpl.contains(SWAPS) && tmpl.at(SWAPS).is_mapping() ) {
          const ordered_node authored_swaps = tmpl.at( SWAPS );
          auto bound = self->bind_swaps_map_to_literals( authored_swaps,
            local_ctx );
          if ( !bound.empty() ) {
            ordered_node swap_node = ordered_node::mapping();
            for ( const auto& kv : bound ) {
              swap_node[ kv.first ] = internal::make_node_from( kv.second );
            }
            obj[ LITERAL_SWAPS ] = swap_node;
          }
        }

        // Identity selection & registration (id -> _id -> locals.id literal)
        bool has_id = obj.is_mapping() && obj.contains( ID )
          && obj.at( ID ).is_string();

        bool has_uid = obj.is_mapping() && obj.contains(AUTO_ID)
          && obj.at(AUTO_ID).is_string();

        bool has_lid = obj.is_mapping() && obj.contains( LOCALS )
          && obj.at( LOCALS ).is_mapping() && obj.at( LOCALS ).contains( ID )
          && obj.at( LOCALS ).at( ID ).is_string();

        const int count_sources = ( has_id ? 1 : 0 )
          + ( has_uid ? 1 : 0 ) + ( has_lid ? 1 : 0 );

        if ( count_sources > 1 ) self->throw_error_at(
          "Conflicting identity fields: {" + ID + ", " + AUTO_ID
            + ", " + LOCALS + PATH_DELIMITER + ID + "}. Choose exactly one."
        );

        std::string elem_scope = section_scope;
        if ( has_id ) {
          const std::string tok
            = internal::to_native_checked< std::string >( obj.at(ID) );
          self->session_.ids.register_token( section_scope, tok, obj );
          elem_scope += PATH_DELIMITER + tok;
        } 
        else if ( has_uid ) {
          const std::string tok
            = internal::to_native_checked< std::string >( obj.at(AUTO_ID) );
          self->session_.ids.register_token( section_scope, tok, obj );
          elem_scope += PATH_DELIMITER + tok;
        }
        else if ( has_lid ) {
          const std::string tok = internal::to_native_checked< std::string >(
            obj.at( LOCALS ).at( ID )
          );
          self->session_.ids.register_token( section_scope, tok, obj );
          elem_scope += PATH_DELIMITER + tok;
        }
        else {
          elem_scope += "[" + std::to_string( i ) + "]";
        }

        // Record child scope for siblings rung
        self->session_.ids.register_child_scope( section_scope, elem_scope );

        // Bind overrides with the instance's local_ctx (params + locals)
        ordered_node bound_broadcast = overrides_broadcast;
        ordered_node bound_per_instance_map = overrides_per_instance_map;
        ordered_node bound_per_instance_seq = overrides_per_instance_seq;

        // Bind strings recursively to materialize placeholders
        internal::bind_strings_recursive( bound_broadcast, local_ctx );
        internal::bind_strings_recursive( bound_per_instance_map, local_ctx );
        internal::bind_strings_recursive( bound_per_instance_seq, local_ctx );

        // Now capture the bound overrides for Resolve to apply
        ResolveSession::OverridesForInstance pack;
        pack.broadcast_map = bound_broadcast;
        pack.per_instance_map = bound_per_instance_map;
        pack.per_instance_seq = bound_per_instance_seq;

        // Decide the conditional overlay now (params/locals available here)
        ordered_node cond = internal::evaluate_conditional_per_instance(
          bound_per_instance_seq, local_ctx );
        if ( cond.is_mapping() && cond.size() > 0 ) {
          pack.conditional_overlay = cond;
        }

        if (pack.has_any()) {
          self->session_.overrides_index[ elem_scope ] = pack;
        }

        // Store the completed instance
        instances.push_back( obj );
      }

      return instances;
    }

    // Mapping walker
    void walk_mapping( ordered_node& node, const std::string& scope ) {
      const std::string saved_scope = self->session_.scope_path;
      self->session_.scope_path = scope;

      // If this mapping itself is a template, then expand it right
      // here into a sequence
      if ( node.is_mapping() && node.contains(TEMPLATE_PARAMETERS) ) {
        // Path stack is already set by the caller to the current key.
        // We will use it in error messages.
        auto concretes = this->expand_template_mapping( node, scope,
          /*path_key_for_errors*/scope );
        node = internal::make_node_from( concretes );
        // After expansion, descend into the resulting sequence
        this->walk_sequence( node, scope,
          /*key_for_path*/ scope.substr(
            scope.find_last_of(PATH_DELIMITER) + 1 )
        );
        self->session_.scope_path = saved_scope;
        return;
      }

      ordered_node out = ordered_node::mapping();
      // Iterate fields of this mapping
      for ( const auto& [mk, mv] : node.map_items() ) {
        const std::string key = mk.get_value< std::string >();
        // Maintain path stack for error reporting
        const std::size_t parent_guard = self->session_.path_stack.size();
        self->session_.path_stack.push_back( key );

        ordered_node child = mv;
        const std::string child_scope
          = scope.empty() ? key : ( scope + PATH_DELIMITER + key );

        // If child is a sequence: register section token and walk elements
        if ( child.is_sequence() ) {
          // Register the sequence 'key' as a section under the parent scope
          self->session_.ids.register_token( scope, key, child );
          self->session_.ids.register_child_scope( scope, child_scope );
          walk_sequence( child, child_scope, key );
          out[ key ] = child;

          while ( self->session_.path_stack.size() > parent_guard ) {
            self->session_.path_stack.pop_back();
          }
          continue;
        }

        // If child is a mapping:
        if ( child.is_mapping() ) {
          const bool is_section
            = internal::mapping_is_section_container( child );

          // Register the section key itself at parent scope
          // so that partial chains like 'locals.standard_files' resolve
          // naturally.
          self->session_.ids.register_token( scope, key, child );
          self->session_.ids.register_child_scope( scope, child_scope );

          // Mapping sections use implicit identity by key; forbid explicit
          // identity in a child mapping.
          // Note: Do not register child tokens here; let recursion register
          // them once, under the correct container branch (sequence or
          // mapping). This avoids duplicate registrations.
          if ( is_section ) {
            for ( const auto& [lk, lv] : child.map_items() ) {
              const std::string childKey = lk.get_value< std::string >();
              if ( lv.is_mapping() ) {
                const bool has_id = lv.contains( ID );
                const bool has_uid = lv.contains( AUTO_ID );
                const bool has_lid = lv.contains( LOCALS )
                  && lv.at( LOCALS ).is_mapping()
                  && lv.at( LOCALS ).contains( ID );

                if ( has_id || has_uid || has_lid ) {
                  std::ostringstream oss;
                  oss << "Mapping section '" << child_scope
                    << "' uses implicit identity by key; child '" << childKey
                    << "' contains explicit identity ('" + ID + "'/'"
                    << AUTO_ID << "'/'" << LOCALS << PATH_DELIMITER << ID
                    << "'), which is not allowed.";
                  self->throw_error_at( oss.str() );
                }
              }
            }
          }

          // If this child mapping itself is a template -> expand here
          if ( child.contains(TEMPLATE_PARAMETERS) ) {
            auto concretes = this->expand_template_mapping( child,
              child_scope, key );
            ordered_node seq = internal::make_node_from( concretes );
            out[ key ] = seq;
            // Continue walking the expanded sequence
            this->walk_sequence( out[key], child_scope, key );

            while ( self->session_.path_stack.size() > parent_guard ) {
              self->session_.path_stack.pop_back();
            }
            continue;
          }

          // Otherwise recurse into the child mapping
          this->walk_mapping( child, child_scope );
          out[ key ] = child;

          while ( self->session_.path_stack.size() > parent_guard ) {
            self->session_.path_stack.pop_back();
          }
          continue;
        }

        // Scalars/null: copy through
        out[ key ] = child;
        while ( self->session_.path_stack.size() > parent_guard ) {
          self->session_.path_stack.pop_back();
        }
      }

      node = out;
      self->session_.scope_path = saved_scope;
    }

    // Sequence walker
    void walk_sequence( ordered_node& seq, const std::string& scope,
      const std::string& key_for_path )
    {
      const std::string saved_scope = self->session_.scope_path;
      self->session_.scope_path = scope;

      std::vector< ordered_node > out;
      out.reserve( seq.size() );

      for ( std::size_t i = 0; i < seq.size(); ++i ) {
        ordered_node elem = seq.at( i );

        // Adjust path stack for index (root....key[i])
        const std::size_t parent_guard = self->session_.path_stack.size();
        const std::string parent_segment = self->session_.path_stack.back();
        self->session_.path_stack.back()
          = internal::seq_indexed( parent_segment, i );

        // If the current element is a mapping-template -> expand and descend
        if ( elem.is_mapping() && elem.contains(TEMPLATE_PARAMETERS) ) {
          auto concretes
            = this->expand_template_mapping( elem, scope, key_for_path );
          for ( auto& c : concretes ) {
            // Determine the child scope: append id/_id/locals.id
            // or [i][j] for element templates
            std::string child_scope = scope;
            bool has_id = c.is_mapping() && c.contains( ID )
              && c.at( ID ).is_string();
            bool has_uid = c.is_mapping() && c.contains( AUTO_ID )
              && c.at( AUTO_ID ).is_string();
            bool has_lid = c.is_mapping() && c.contains( LOCALS )
              && c.at( LOCALS ).is_mapping() && c.at( LOCALS ).contains( ID )
              && c.at( LOCALS ).at( ID ).is_string();

            if (has_id) {
              child_scope += PATH_DELIMITER
                + internal::to_native_checked< std::string >( c.at(ID) );
            }
            else if (has_uid) {
              child_scope += PATH_DELIMITER
                + internal::to_native_checked< std::string >( c.at(AUTO_ID) );
            }
            else {
              // fall back to index label chain like old nested template path
              child_scope += '[' + std::to_string( i ) + ']';
            }

            // Register child scope and descend
            self->session_.ids.register_child_scope( scope, child_scope );
            this->walk_mapping( c, child_scope );
            out.push_back( c );
          }

          // restore parent path segment
          self->session_.path_stack.back() = parent_segment;
          while ( self->session_.path_stack.size() > parent_guard ) {
            self->session_.path_stack.pop_back();
          }
          continue;
        }

        // Concrete element: detect identity, register token, compute child
        // scope, and then descend
        std::string elem_scope = scope;
        bool has_id = elem.is_mapping() && elem.contains( ID )
          && elem.at( ID ).is_string();
        bool has_uid = elem.is_mapping() && elem.contains( AUTO_ID )
          && elem.at( AUTO_ID ).is_string();
        bool has_lid = elem.is_mapping() && elem.contains( LOCALS )
          && elem.at( LOCALS ).is_mapping() && elem.at( LOCALS ).contains( ID )
          && elem.at( LOCALS ).at( ID ).is_string();

        // If this element has an identity, then register it
        // under the sequence scope
        if ( has_id ) {
          const std::string tok
            = internal::to_native_checked< std::string >( elem.at(ID) );
          self->session_.ids.register_token( scope, tok, elem );
          elem_scope += PATH_DELIMITER + tok;
        }
        else if ( has_uid ) {
          const std::string tok
            = internal::to_native_checked< std::string >( elem.at(AUTO_ID) );
          self->session_.ids.register_token( scope, tok, elem );
          elem_scope += PATH_DELIMITER + tok;
        }
        else if ( has_lid ) {
          const std::string tok = internal::to_native_checked< std::string >(
            elem.at( LOCALS ).at( ID )
          );
          self->session_.ids.register_token( scope, tok, elem );
          elem_scope += PATH_DELIMITER + tok;
        }
        else {
          // No identity -> index fallback
          elem_scope += '[' + std::to_string( i ) + ']';
        }

        // Record child scope (siblings rung)
        self->session_.ids.register_child_scope( scope, elem_scope );

        // Descend
        if ( elem.is_mapping() ) {
          this->walk_mapping( elem, elem_scope );
        } else if ( elem.is_sequence() ) {
          this->walk_sequence( elem, elem_scope, key_for_path );
        }

        // After descent, update canonical node so later base clones see the
        // final concretized object
        self->session_.ids.canonical_nodes[ elem_scope ] = elem;

        // Restore path segment and push to out
        self->session_.path_stack.back() = parent_segment;
        out.push_back( elem );

        while ( self->session_.path_stack.size() > parent_guard ) {
          self->session_.path_stack.pop_back();
        }
      }

      seq = internal::make_node_from( out );
      self->session_.scope_path = saved_scope;
    }
  }; // struct Walker

  if ( !doc_.is_mapping() ) return;

  // Begin recursive Concretize from root. The path/scope are already
  // initialized in resolve(...)
  Walker w{ this };
  w.walk_mapping( doc_, internal::DOC_ROOT );
}

inline std::unordered_map< std::string, std::string >
  yodel::Resolver::bind_swaps_map_to_literals(
    const ordered_node& swaps_map,
    const std::unordered_map< std::string, std::string >& ctx )
{
  std::unordered_map< std::string, std::string > out;
  if ( !swaps_map.is_mapping() ) return out;

  for ( const auto& [mk, mv] : swaps_map.map_items() ) {
    const std::string key_bound
      = internal::protected_bind( mk.get_value< std::string >(), ctx );
    const std::string val_bound
      = internal::protected_bind( internal::to_string_any(mv), ctx );
    // Last write wins for bound key collisions
    out[ key_bound ] = val_bound;
  }
  return out;
}

inline void yodel::Resolver::apply_literal_swaps_shallow( ordered_node& node,
  const std::unordered_map<std::string, std::string>& swaps )
{
  using internal::AUTO_LOCAL_PREFIX;
  using internal::BASE;
  using internal::ID;
  using internal::LOCALS;
  using internal::OVERRIDES;
  using internal::SWAPS;

  if ( swaps.empty() ) return;

  if ( node.is_mapping() ) {
    for ( auto& [mk, mv] : node.map_items() ) {
      const std::string key = mk.get_value< std::string >();
      // Protected/meta/markers
      if ( key == ID || key == BASE || key == LOCALS
        || key == OVERRIDES || key == SWAPS
        || (!key.empty() && key[0] == AUTO_LOCAL_PREFIX) )
      {
        continue;
      }
      if ( mv.is_string() ) {
        const std::string val = mv.get_value< std::string >();
        auto it = swaps.find( val );
        if ( it != swaps.end() ) mv = internal::make_node_from( it->second );
      }
      // Do not recurse; children will apply swaps themselves when visited
    }
  }
  else if ( node.is_sequence() ) {
    for ( size_t i = 0; i < node.size(); ++i ) {
      ordered_node elem = node.at( i );
      if ( elem.is_string() ) {
        const std::string val = elem.get_value< std::string >();
        auto it = swaps.find( val );
        if ( it != swaps.end() ) {
          node[ i ] = internal::make_node_from( it->second );
        }
      }
      // Mapping/sequence children: not recursing here
    }
  }
  // Scalars: nothing to do
}

// Resolve-time identity selection for sequence overlays (id -> _id -> bound locals.id).
// Policy enforces whether locals.id must be literal (Concretize) or may be bound (Resolve).
inline std::optional<std::string>
  yodel::Resolver::element_identity_token( const ordered_node& el,
    const std::unordered_map< std::string, std::string >& ctx,
    IdentityPolicy policy )
{
  using internal::AUTO_ID;
  using internal::ID;
  using internal::LOCALS;

  if ( !el.is_mapping() ) return std::nullopt;

  if ( el.contains(ID) && el.at(ID).is_string() ) {
    return internal::to_native_checked< std::string >( el.at(ID) );
  }

  if ( el.contains(AUTO_ID) && el.at(AUTO_ID).is_string() ) {
    return internal::to_native_checked< std::string >( el.at(AUTO_ID) );
  }

  if ( el.contains(LOCALS) && el.at(LOCALS).is_mapping()
    && el.at(LOCALS).contains(ID) )
  {
    ordered_node lid = el.at( LOCALS ).at( ID );
    if ( !lid.is_string() ) return std::nullopt;
    const std::string raw = internal::to_native_checked< std::string >( lid );

    if ( policy == IdentityPolicy::RequireLiteralInConcretize ) {
      // Concretize: locals.id must be literal; reject if true placeholders
      // exist
      if ( internal::contains_true_placeholder(raw) ) return std::nullopt;
      else return raw;
    }
    else {
      // Resolve: bind with inherited context; reject if placeholders remain
      const std::string bound = internal::protected_bind( raw, ctx );
      if ( internal::contains_true_placeholder(bound) ) return std::nullopt;
      return bound;
    }
  }
  return std::nullopt;
}

inline void yodel::Resolver::apply_sequence_overlays_by_id(
  ordered_node& base_seq, const ordered_node& overlay_map )
{
  if ( !base_seq.is_sequence() || !overlay_map.is_mapping() ) return;

  // Build overlay map id -> overlay node; collect required ids
  std::unordered_map< std::string, ordered_node > overlay_by_id;
  std::unordered_set< std::string > required_ids;
  for ( const auto& [idk, idv] : overlay_map.map_items() ) {
    const std::string id = idk.get_value< std::string >();
    overlay_by_id[ id ] = idv;
    required_ids.insert( id );
  }

  // Resolve-time context: merge root frame + all ctx_stack frames on top
  std::unordered_map< std::string, std::string >
    inherited_ctx = session_.root_ctx;
  for ( const auto& frame : session_.ctx_stack ) {
    for ( const auto& kv : frame ) inherited_ctx[ kv.first ] = kv.second;
  }

  // Merge overlays into sequence elements by id; track resolved ids
  std::unordered_set< std::string > resolved_ids;
  std::vector< ordered_node > new_seq;
  new_seq.reserve( base_seq.size() );

  for ( std::size_t si = 0; si < base_seq.size(); ++si ) {
    ordered_node el = base_seq.at( si );
    auto eid_opt = this->element_identity_token( el, inherited_ctx,
      IdentityPolicy::AllowBoundLocalsInResolve );
    if ( eid_opt ) {
      const std::string& eid = *eid_opt;
      auto it = overlay_by_id.find( eid );
      if ( it != overlay_by_id.end() ) {
        el = internal::deep_merge( el, it->second );
        resolved_ids.insert( eid );
      }
    }
    new_seq.push_back( el );
  }

  // Strict unknown ids
  for ( const auto& need : required_ids ) {
    if ( !resolved_ids.count(need) ) {
      std::ostringstream msg;
      msg << "Unified: sequence-by-id override targets unknown"
        << " id '" << need << "'";
      throw_error_at( msg.str() );
    }
  }

  // Order preserved
  base_seq = internal::make_node_from( new_seq );
}

// Shared for object-level and inline field-level
// 'base' merges; guardrails enforced.
inline void yodel::Resolver::resolve_and_merge_base(
  ordered_node& target_map, const std::string& current_object_canon )
{
  using internal::BASE;

  if ( !target_map.is_mapping() || !target_map.contains(BASE) ) return;
  const ordered_node baseNode = target_map.at( BASE );
  if ( !baseNode.is_string() ) {
    throw_error_at( "'" + BASE + "' inside field/object must be a"
      " string reference token." );
  }

  // Resolve-time context: merge root + ctx_stack
  std::unordered_map< std::string, std::string > inherited_ctx
    = session_.root_ctx;
  for ( const auto& frame : session_.ctx_stack ) {
    for ( const auto& kv : frame ) inherited_ctx[ kv.first ] = kv.second;
  }

  // Bind token and resolve to canonical identity
  std::string tok = internal::protected_bind(
    baseNode.get_value< std::string >(), inherited_ctx );
  std::string canon;
  const internal::TokenKind kind = internal::classify_token( tok );
  try {
    if ( kind == internal::TokenKind::Qualified ) {
      auto c = session_.ids.resolve_qualified( tok );
      if ( !c ) throw std::runtime_error( "unknown qualified base" );
      canon = *c;
    }
    else if ( kind == internal::TokenKind::Partial ) {
      canon = session_.ids.resolve_partial_chain( session_.scope_path,
        internal::split_segments(tok) );
    }
    else {
      canon = session_.ids.resolve_via_ladder( session_.scope_path, tok );
    }
  }
  catch ( const std::exception& ) {
    std::ostringstream oss;
    oss << "Unknown base '" << tok << "'";
    const char* hint = ( kind == internal::TokenKind::Qualified
      ? "resolution: qualified" : "resolution: ladder" );
    throw_error_at( oss.str(), std::optional< std::string >(hint) );
  }

  // Guardrails: self / descendant / cycles
  if ( !current_object_canon.empty() ) {
    if ( canon == current_object_canon ) {
      std::ostringstream oss;
      oss << "Inline/object-level base resolves to the current"
        << " object ('" << current_object_canon
        << "'). An object cannot inherit from itself.";
      throw_error_at( oss.str() );
    }
    if ( canon.rfind(current_object_canon + internal::PATH_DELIMITER, 0) == 0)
    {
      std::ostringstream oss;
      oss << "Inline/object-level base resolves to a descendant ('" << canon
        << "'). Inheriting from descendants is not allowed.";
      throw_error_at( oss.str() );
    }
  }
  if ( session_.visited_bases_chain.count(canon) ) {
    std::ostringstream oss;
    oss << "Inline base cycle detected: '" << canon
      << "' already used in this inheritance chain";
    throw_error_at( oss.str() );
  }
  session_.visited_bases_chain.insert( canon );

  // Clone base and deep-merge overlay (target_map minus 'base')
  ordered_node base_clone = session_.ids.canonical_nodes.at( canon );
  ordered_node overlay = ordered_node::mapping();
  for ( const auto& [mk, mv] : target_map.map_items() ) {
    const std::string k = mk.get_value< std::string >();
    if ( k == BASE ) continue;
    overlay[ k ] = mv;
  }
  ordered_node merged = internal::deep_merge( base_clone, overlay );

  // Strip any 'base' key carried from cloned base
  if (merged.is_mapping() && merged.contains(BASE) ) {
    ordered_node cleaned = ordered_node::mapping();
    for ( const auto& [mk, mv] : merged.map_items() ) {
      const std::string k = mk.get_value<std::string>();
      if ( k == BASE ) continue;
      cleaned[k] = mv;
    }
    target_map = cleaned;
  }
  else {
    target_map = merged;
  }

  // Remove from provenance list if present
  for ( const auto& [mk, mv] : overlay.map_items() ) {
    const std::string k = mk.get_value< std::string >();
    internal::remove_from_base_keys_if_present( target_map, k );
  }

  // Pop from visited set at end of this merge step (no recursion here)
  session_.visited_bases_chain.erase( canon );
}

// Resolve-mode binder: multi-pass binding with inherited
// context (nearest-wins). Concretize mode will be used only for write
// sites; Resolve uses the context stack.
inline void yodel::Resolver::bind_strings_multi_pass( ordered_node& obj,
  BinderMode mode )
{
  using internal::OPEN_PLACEHOLDER;
  using internal::CLOSE_PLACEHOLDER;

  if ( !obj.is_mapping() ) return;

  // Concretize binder is used at write sites (Step 5); no multi-pass here.
  // This entry is provided for completeness; nothing to do in Step 6.
  if ( mode == BinderMode::Concretize ) return;

  // Resolve: multi-pass binding with inherited context frames
  bool unresolved = false;
  for ( int pass = 0; pass < max_bind_passes_; ++pass ) {
    unresolved = false;

    // Build merged context: root + frames in order, then current object's own
    // scalar/string fields
    std::unordered_map< std::string, std::string > merged = session_.root_ctx;
    for ( const auto& frame : session_.ctx_stack ) {
      for ( const auto& kv : frame ) merged[ kv.first ] = kv.second;
    }
    for ( const auto& [mk, mv] : obj.map_items() ) {
      const std::string key = mk.get_value< std::string >();
      if ( internal::is_non_null_scalar(mv) ) {
        merged[ key ] = internal::to_string_any( mv );
      }
    }

    // Bind strings
    for ( const auto& [mk, mv] : obj.map_items() ) {
      if ( !mv.is_string() ) continue;
      const std::string key = mk.get_value< std::string >();
      const std::string s = mv.get_value< std::string >();

      // Skip strings with no detected placeholders (no binding needed)
      if ( s.find(OPEN_PLACEHOLDER) == std::string::npos
        && s.find(CLOSE_PLACEHOLDER + CLOSE_PLACEHOLDER) == std::string::npos )
      {
        continue;
      }

      std::string t = internal::protected_bind( s, merged );
      if ( internal::contains_true_placeholder(t) ) unresolved = true;
      obj[ key ] = internal::make_node_from( t );
    }

    if ( !unresolved ) break;
  }

  if ( unresolved ) {
    // Report unresolved placeholders with names
    for ( const auto& [mk, mv] : obj.map_items() ) {
      if ( !mv.is_string() ) continue;
      const std::string key = mk.get_value< std::string >();
      const std::string s = mv.get_value< std::string >();

      std::string t = s;
      internal::replace_all( t, OPEN_PLACEHOLDER + OPEN_PLACEHOLDER, "\x01" );
      internal::replace_all( t, CLOSE_PLACEHOLDER + CLOSE_PLACEHOLDER, "\x02" );

      static const std::regex ph( R"(\)" + OPEN_PLACEHOLDER
      + R"("[A-Za-z_][A-Za-z0-9_]*\)" + CLOSE_PLACEHOLDER );
      std::smatch m;
      auto begin = t.cbegin();
      std::unordered_set< std::string > names;
      while ( std::regex_search(begin, t.cend(), m, ph) ) {
        names.insert( m[1].str() );
        begin = m.suffix().first;
      }
      if ( !names.empty() ) {
        std::ostringstream msg;
        msg << "Unified: unresolved placeholder(s) in field '" << key << "': ";
        bool first = true;
        for ( const auto& nm : names ) {
          if (!first) msg << ", ";
          msg << OPEN_PLACEHOLDER << nm << CLOSE_PLACEHOLDER;
          first = false;
        }
        throw_error_at( msg.str() );
      }
    }
  }
}

// Full Resolve traversal with overrides (per-instance -> broadcast
// -> concrete), base merges (object-level + inline),
// swaps (child-wins, shallow), binding, and canonical identity updates.
inline void yodel::Resolver::resolve_unified() {

  using internal::LITERAL_SWAPS;
  using internal::OVERRIDES;
  using internal::PATH_DELIMITER;
  using internal::SWAPS;

  if ( !doc_.is_mapping() ) return;

  // Build root context once
  session_.root_ctx = internal::collect_root_context( doc_ );

  // Recursive walker implemented as an inner struct
  struct Walker {
    Resolver* self;

    void walk_mapping( ordered_node& node, const std::string& scope,
      const std::unordered_map< std::string, std::string >& parent_swaps )
    {
      // Temporarily set the scope path while walking a mapping
      const std::string saved_scope = self->session_.scope_path;
      self->session_.scope_path = scope;

      // Push this mapping's scalar/string fields into context (nearest wins)
      self->ctx_push( node );

      // Apply per-instance and broadcast overrides (if any) keyed by current
      // scope
      auto itPack = self->session_.overrides_index.find( scope );
      if ( itPack != self->session_.overrides_index.end() ) {
        const auto& pack = itPack->second;

        // Helper to apply one override map (field -> overlay)
        auto apply_override_map = [&]( const ordered_node& ovmap ) {
          if ( !ovmap.is_mapping() ) return;
          for ( const auto& [ok, ovv] : ovmap.map_items() ) {
            const std::string k = ok.get_value< std::string >();

            // Sequence-by-id form
            if ( node.contains(k) && node.at(k).is_sequence()
              && ovv.is_mapping() )
            {
              ordered_node seq = node.at( k );
              self->apply_sequence_overlays_by_id( seq, ovv );
              node[ k ] = seq;
              internal::remove_from_base_keys_if_present( node, k );
              continue;
            }

            // Deep-merge for mapping values; scalars & sequences replace; null
            // clears
            if ( ovv.is_mapping() && node.contains(k)
              && node.at(k).is_mapping() )
            {
              node[ k ] = internal::deep_merge( node.at(k), ovv );
            }
            else {
              node[ k ] = ovv; // replace or clear
            }
            internal::remove_from_base_keys_if_present( node, k );
          }
        };

        // Override application order specified in the YODEL grammar:
        // per-instance -> broadcast
        if ( pack.per_instance_map.is_mapping() ) {

          // Each field maps to a sequence of overlays (per index) that have
          // already been captured. For the Resolve step, we apply the overlay
          // for this instance only if authored as a map-on-sequence.
          apply_override_map( pack.per_instance_map );
        }
        // Apply the precomputed conditional overlay (evaluated during
        // Concretize).
        if ( pack.conditional_overlay.is_mapping()
          && pack.conditional_overlay.size() > 0 )
        {
          apply_override_map( pack.conditional_overlay );
        }
        if ( pack.broadcast_map.is_mapping() ) {
          apply_override_map( pack.broadcast_map );
        }
      }

      // Apply concrete object-level overrides on this mapping (if present)
      if ( node.contains(OVERRIDES) && node.at(OVERRIDES).is_mapping() ) {
        const ordered_node overrides = node.at( OVERRIDES );
        // Iterate override keys
        for ( const auto& [ok, ovv] : overrides.map_items() ) {
          const std::string k = ok.get_value< std::string >();

          // Sequence-by-id form
          if ( node.contains(k) && node.at(k).is_sequence()
            && ovv.is_mapping() )
          {
            ordered_node seq = node.at( k );
            self->apply_sequence_overlays_by_id( seq, ovv );
            node[ k ] = seq;
            internal::remove_from_base_keys_if_present( node, k );
            continue;
          }

          // Deep-merge for mapping values; scalars/sequences replace; null
          // clears
          if ( ovv.is_mapping() && node.contains(k)
            && node.at(k).is_mapping() )
          {
            node[ k ] = internal::deep_merge( node.at(k), ovv );
          }
          else {
            node[ k ] = ovv;
          }
          internal::remove_from_base_keys_if_present( node, k );
        }
      }

      // Refresh context frame after overrides
      self->ctx_pop(); // drop stale frame captured before overrides
      self->ctx_push( node ); // rebuild frame from the updated mapping

      // Object-level base merges
      self->resolve_and_merge_base( node, scope );

      // Refresh context frame after object-level base merge
      self->ctx_pop(); // drop frame built before base merge
      self->ctx_push( node ); // rebuild with inherited fields now present

      // Inline field-level base merges: for each mapping value with 'base'
      ordered_node pre = node; // iterate through original keys
      for ( const auto& [mk, mv] : pre.map_items() ) {
        const std::string k = mk.get_value< std::string >();
        if ( !mv.is_mapping() ) continue;
        ordered_node v = mv;
        self->resolve_and_merge_base( v, scope );
        node[ k ] = v;
      }

      // Compose effective swaps: inherit parent (child-wins cascade), then
      // overlay local maps (overlay-authored _literal_swaps are inherited)
      std::unordered_map< std::string, std::string >
        effective_swaps = parent_swaps;
      if ( node.contains( LITERAL_SWAPS )
        && node.at( LITERAL_SWAPS ).is_mapping() )
      {
        const ordered_node& smap = node.at( LITERAL_SWAPS );
        for ( const auto& [sk, sv] : smap.map_items() ) {
          effective_swaps[ sk.get_value< std::string >() ]
            = internal::to_string_any( sv ); // child overlays parent
        }
      }

      // Overlay authored swaps (bound with Resolve context)
      {
        std::unordered_map< std::string, std::string >
          inherited = self->session_.root_ctx;
        for ( const auto& frame : self->session_.ctx_stack ) {
          for ( const auto& kv : frame ) inherited[ kv.first ] = kv.second;
        }
        if ( node.contains(SWAPS) && node.at(SWAPS).is_mapping() ) {
          auto bound = self->bind_swaps_map_to_literals(
            node.at(SWAPS), inherited );
          for ( const auto& kv : bound ) {
            effective_swaps[ kv.first ] = kv.second; // child overlays parent
          }
        }
      }

      // Apply swaps shallowly to this object
      self->apply_literal_swaps_shallow( node, effective_swaps );

      // Also apply shallowly to immediate sequence fields of this mapping
      {
        ordered_node snapshot = node; // iterate original keys
        for ( const auto& [mk, mv] : snapshot.map_items() ) {
          const std::string k = mk.get_value< std::string >();
          if ( mv.is_sequence() ) {
            ordered_node seq = mv;
            self->apply_literal_swaps_shallow( seq, effective_swaps );
            node[ k ] = seq; // write back swapped sequence
          }
        }
      }

      // Bind strings with Resolve context (multi-pass)
      self->bind_strings_multi_pass( node, BinderMode::Resolve );

      // Update canonical_nodes so later bases see the resolved object
      if ( !scope.empty() ) {
        self->session_.ids.canonical_nodes[ scope ] = node;
      }

      // Recurse into children (containers only)
      for ( const auto& [mk, mv] : node.map_items() ) {
        const std::string k = mk.get_value< std::string >();
        ordered_node child = mv;

        // Maintain path/scope for errors and
        // ladder resolution (record before push)
        const std::size_t parent_guard = self->session_.path_stack.size();
        self->session_.path_stack.push_back( k );

        std::string child_scope = (child.is_mapping() || child.is_sequence())
          ? ( scope.empty() ? k : (scope + PATH_DELIMITER + k) ) : scope;

        if ( child.is_mapping() ) {
          this->walk_mapping( child, child_scope, effective_swaps );
        }
        else if ( child.is_sequence() ) {
          this->walk_sequence( child, child_scope, k, effective_swaps );
        }

        node[ k ] = child;
        while ( self->session_.path_stack.size() > parent_guard ) {
          self->session_.path_stack.pop_back();
        }
      }

      // Pop context frame for this mapping
      self->ctx_pop();

      // Restore previous scope path now that the walk is over
      self->session_.scope_path = saved_scope;
    }

    void walk_sequence( ordered_node& seq, const std::string& scope,
      const std::string& key_for_path,
      const std::unordered_map< std::string, std::string >& parent_swaps )
    {
      // Temporarily set the scope path while walking a sequence
      const std::string saved_scope = self->session_.scope_path;
      self->session_.scope_path = scope;

      // Swaps apply to sequence scalars before descending
      // (use inherited parent swaps)
      self->apply_literal_swaps_shallow( seq, parent_swaps );

      std::vector< ordered_node > out;
      out.reserve( seq.size() );

      // Resolve-time context: merged for identity resolution
      std::unordered_map< std::string, std::string >
        inherited = self->session_.root_ctx;

      for ( const auto& frame : self->session_.ctx_stack ) {
        for ( const auto& kv : frame ) inherited[ kv.first ] = kv.second;
      }

      for ( std::size_t i = 0; i < seq.size(); ++i ) {
        ordered_node v = seq.at( i );

        // Remember the parent's last path segment ("... .key_for_path")
        const std::string parent_segment = self->session_.path_stack.back();

        // Temporarily replace the parent's last segment with "key_for_path[i]"
        self->session_.path_stack.back()
          = internal::seq_indexed( parent_segment, i );

        // Build element scope (append id if present; else use [i])
        std::string elem_scope = scope;
        auto sid_opt = self->element_identity_token(v, inherited,
          IdentityPolicy::AllowBoundLocalsInResolve );

        if ( sid_opt ) {
          const std::string& idStr = *sid_opt;
          const bool already_has_id = (
            !elem_scope.empty() && elem_scope.size() >= idStr.size() + 1
            && elem_scope.compare(elem_scope.size() - idStr.size(),
                 idStr.size(), idStr) == 0
            && elem_scope[ elem_scope.size() - idStr.size() - 1 ]
              == PATH_DELIMITER
          );
          if ( !already_has_id ) {
            elem_scope = elem_scope.empty()
              ? idStr : ( elem_scope + PATH_DELIMITER + idStr );
          }
        }
        else {
          elem_scope = elem_scope + '[' + std::to_string( i ) + ']';
        }

        // Descend if the element is either a mapping or a sequence
        if ( v.is_mapping() ) {
          this->walk_mapping( v, elem_scope, parent_swaps );
        }
        else if ( v.is_sequence() ) {
          this->walk_sequence( v, elem_scope, key_for_path, parent_swaps );
        }

        // Restore the parent's path segment
        self->session_.path_stack.back() = parent_segment;

        out.push_back( v );
      }

      seq = internal::make_node_from( out );

      // Restore previous scope path now that the walk is over
      self->session_.scope_path = saved_scope;
    }
  };

  Walker w{ this };
  // Start at root mapping with scope "root"
  w.walk_mapping( doc_, internal::DOC_ROOT, /*parent_swaps=*/ {} );
}

inline void yodel::Resolver::prune_final() {

  using internal::AUTO_LOCAL_PREFIX;
  using internal::LOCALS;
  using internal::LOCALS_KEYS;
  using internal::OVERRIDES;
  using internal::SWAPS;

  // Recursive prune: drop nulls, locals, meta control sections, markers, and
  // keys in _locals_keys
  // Note: we use a lambda here to allow easy recursion without requiring
  // an input ordered_node& in the class-level interface
  std::function< void( ordered_node& node ) > prune
    = [&]( ordered_node& node ) -> void
  {
    if ( node.is_mapping() ) {
      // Collect _locals_keys
      std::unordered_set< std::string > locals_keys;
      if ( node.contains(LOCALS_KEYS) ) {
        const ordered_node &lk = node.at( LOCALS_KEYS );
        if ( lk.is_sequence() ) {
          for ( std::size_t i = 0; i < lk.size(); ++i ) {
            const ordered_node& nameNode = lk.at( i );
            if ( nameNode.is_string() ) {
              locals_keys.insert(
                internal::to_native_checked< std::string >( nameNode )
              );
            }
          }
        }
      }

      ordered_node pruned = ordered_node::mapping();
      for ( const auto& [mk, mv] : node.map_items() ) {
        const std::string k = mk.get_value< std::string >();

        // Drop nulls
        if ( mv.is_null() ) continue;

        // Prune locals & immediate meta
        if ( k == LOCALS || k == OVERRIDES || k == SWAPS ) continue;

        // Drop markers (_locals_keys, _base_keys, _literal_swaps, etc.)
        if ( !k.empty() && k[0] == AUTO_LOCAL_PREFIX ) continue;

        // Prune any field listed in _locals_keys
        if ( locals_keys.count(k) ) continue;

        ordered_node v = mv;
        prune( v );
        pruned[ k ] = v;
      }
      node = pruned;
    }
    else if ( node.is_sequence() ) {
      std::vector< ordered_node > out;
      out.reserve( node.size() );
      for ( std::size_t i = 0; i < node.size(); ++i ) {
        ordered_node v = node.at( i );
        prune( v );
        out.push_back( v );
      }
      node = internal::make_node_from( out );
    }
    // Scalars: nothing to do
  };

  prune( doc_ );
}
