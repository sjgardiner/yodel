==================================================
YODEL: YAML Object Derivation & Expansion Language
==================================================

A generic, YAML-based configuration language with structured inheritance,
template expansion, and deterministic resolution.

Overview
--------
Any YAML mapping can be:
- A **concrete** object,
- A **template** (declared by ``template parameters``) that expands to multiple concretes,
- A child object nested under any other object.

The language supports:
- Single-base inheritance (``base``),
- Single-phase string binding,
- Locals (including underscore auto-locals) with pruning,
- Template-level and per-instance conditional overrides,
- Nested templates,
- Ordered expansion and deterministic resolution,
- Scope-first identity lookups with a parent-chain ladder.

Identity
--------
- **Mapping sections (by key):** Child objects under a mapping **implicitly** receive
  identity from their **key**.

  Example::

    detectors:
      lar_v83:
        meta: { kind: "LArTPC" }

  The child’s implicit token is ``lar_v83``. Its canonical identity is
  ``root.detectors.lar_v83``.

  **Rule:** Inside a **mapping section**, the field named ``id`` is **forbidden**.
  Identity is the key. If element-level identity is required, use a **sequence section**
  and declare explicit ``id`` on elements.  (Explicit identity forms are forbidden
  inside mapping-section children.)

- **Sequences:** Elements intended to be referenced **must** declare an explicit ``id``.
  Identity for sequence elements is registered after Concretize completes for the
  element, using the **final bound** value under the precedence rule.

Container token registration (mapping sections)
----------------------------------------------

In addition to registering child identities by key under a mapping section, implementations
MUST also register the **section key itself** as a resolvable token at the parent scope.

Semantics:

- The registered token refers to the **container mapping** (the section as a whole), not to a specific child.
- Partial chains like ``locals.standard_files`` MUST resolve to the container mapping if ``standard_files``
  is authored as a mapping section under ``locals``.
- This registration MUST NOT alter identity rules of mapping-section children: child identity remains the
   child’s **key**, and explicit identity forms (``id``, ``_id``) remain forbidden inside mapping-section
  children.
- When resolving partially-scoped tokens via the parent-chain ladder, normal ambiguity rules apply per rung.

Rationale:
- Improves ergonomics for authors: container-level references become explicit and discoverable.
- Avoids implementation-specific shadowing surprises for common chains that name containers.

Container token registration (sequence sections)
------------------------------------------------

In addition to registering element identities under a sequence section (via explicit ``id``),
implementations MUST also register the **sequence field key itself** as a resolvable token at the
parent scope.

Semantics:

- The registered token refers to the **container sequence** (the entire list), not to any individual element.
- Partial chains like ``runs.universes`` MUST resolve to the sequence container if ``universes`` is authored
  as a sequence section under ``runs``.
- This registration MUST NOT alter identity rules for sequence elements: element identity remains
   explicit via ``id`` (or ``_id`` under the usual precedence), and index-based fallback
  continues for error paths.
- When resolving partially-scoped tokens via the parent-chain ladder, normal ambiguity rules apply per rung.

Rationale:
- Improves ergonomics for authors: container-level references become explicit and discoverable.
- Aligns sequence sections with mapping sections for consistency in partial-chain resolution.
- Avoids implementation-specific gaps where container-level references might fail unexpectedly.

Identity sources (string-only)
------------------------------
Authors may provide exactly one of:
- ``id`` — persists in final output; ``{id}`` binds like any other name.
- ``_id`` — treated as an auto-local and **pruned**; ``{_id}`` binds (no aliasing).

**Collision (site-local):** Exactly one of {``id``, ``_id``} may be present.
If both exist, the resolver errors.

**Mapping sections forbiddance:** Inside a mapping section child, all explicit identity
forms {``id``, ``_id``} are **forbidden**; identity is the child key.

Identity storage vs. error paths
--------------------------------
- **Indexing/lookup** uses a single **identity scope** string (e.g., ``root.runs.Run1.universes``).
- **Error paths** remain index-based for clarity (e.g., ``root.runs[0].universes[3]``),
  but these index segments are **not** used for identity registration/lookup.

Sequence scope semantics
------------------------
During Resolve, when descending into a sequence element:
- If the element has an ``id``, the resolver **appends that id** to the scope (e.g., ``root.runs.Run1``),
  so nested unqualified references (e.g., ``base: CV`` within ``universes``) start
  at the **enclosing object scope**.
- If the element has **no** ``id``, a unique index fallback scope (e.g., ``[i]``) is used.

Contextual references
---------------------
References are plain strings interpreted **only** in contextual slots:
- **Reference-typed field:** ``base``

  - **Object-level** (on templates/concretes): ``base: "token"``

Literals
--------
All other string fields are **literals** under this grammar.

Placeholders
------------
Placeholders ``{name}`` may appear inside string literals and reference tokens.

Identity placeholders (no aliasing)
-----------------------------------
- ``{id}`` binds iff identity is authored via **``id``**.
- ``{_id}`` binds iff identity is authored via **``_id``**.
- No aliasing between names; no synthetic markers are introduced.
- Doubled-brace protection remains unchanged.

Single-phase string binding
---------------------------
All string fields are bound in a single pass during template expansion.

Application order & precedence (strongest → weakest):

1. **Locals** (computed from parameters and previously bound locals),
2. **Instance fields** (per-index),
3. **Inline fields** (defaults).

Precedence rule (field-agnostic defaults):
- Inline fields act as **defaults**. When applying inline fields, the resolver sets a field
  **only if** the field is currently **absent**, explicitly **null**, or the current value
  still originates from the **base clone** of the instance (dynamic provenance).
- Inline defaults SHALL NOT overwrite values introduced by previous steps (1–2) that come
  from the template itself.

Base-derived fields & dynamic provenance:
- When a template instance inherits from a base object, the resolver marks the keys present
  in the base clone (e.g., an internal list). As expansion writes fields (locals / instance
  fields), any written field MUST be removed from that provenance list for the instance.
- Inline defaults MAY refine/replace only fields that **remain** in the provenance list,
  or fields that are **absent/null**.

Explicit clears:
- Authors MAY set a field to ``null`` in instance material to signal that inline defaults
  should populate that field later in the same pass.

**Unified pass after expansion** performs:

  1) **Template-level and per-instance conditional overrides**:

     - Mapping values **deep-merge** (override keys win; unspecified keys preserved),
     - Scalar and sequence values **replace**,
     - Setting a subkey to ``null`` **removes** it,
     - **Sequence-by-id** override form is applied here,
       with strict unknown-id checks and order preservation.

  2) **Object-level base** merges for concretes (guardrails; clone final base and deep-merge),
  3) **Concrete object-level overrides** (including sequence-by-id targeting),
  4) **Append** (add new elements to inherited or resolved sequences),
  5) **Binding of plain string placeholders** using an **inherited context**,
  6) **Pruning** of locals (including underscore auto-locals) and meta,
     and removal of provenance markers.

Inherited context
------------------
- Starts with **root.locals** plus other **root-level** non-section string/scalar fields,
- Accumulates **ancestor** mapping fields (string and scalar) as the walk descends,
- Uses **nearest-wins** shadowing: a child field overrides the same key from ancestors/global context.

Binding in the unified pass repeats until stable or the pass limit is reached. If any placeholder remains, resolution fails.

Doubled-brace literals
----------------------
When binding, implementations MUST protect doubled braces before placeholder expansion
and restore them afterward:
- ``{{`` → ``{`` (literal open brace)
- ``}}`` → ``}`` (literal close brace)

Resolution (qualified vs. partial vs. unqualified)
--------------------------------------------------
- **Qualified tokens:** MUST start with ``root.`` (scope anchor). Resolution is **absolute**.
  If no canonical identity matches exactly, resolution fails with a qualified hint.

- **Partially-scoped tokens:** contain at least one ``.`` but do **not** start with ``root.``.
  These are **relative chains** resolved via the **parent-chain ladder** beginning at the
  **enclosing object scope**. At each ladder rung (current scope → parent → grandparent → …),
  the resolver attempts the entire chain. If a rung yields **exactly one** complete chain match,
  resolution succeeds. If a rung yields **more than one** complete chain match, the reference is
  **ambiguous** and resolution fails, listing **only that rung's matches**.
  If no rung matches, resolution fails with a ladder hint.

- **Unqualified tokens:** contain **no ``.``**. These resolve via the **same ladder**, but match a
  **single symbol** per rung. Ambiguity and ladder errors behave as described above.

Anchor note:
- The string ``root`` is the scope anchor **only** when used as the leading segment ``root.``
  in a token. Elsewhere (e.g., a bare token ``root`` or a chain segment like ``... .root . ...``),
  it is a **normal symbol** that may be defined and resolved like any other token.

Inheritance
-----------
**Object-level** ``base`` (templates/concretes):
- Must be authored as a **string** token.
- For templates, the base object must be defined **earlier** in the document (base-before-template).
- Placeholders are bound **per instance** using the template's parameter context **before** resolving the base.

Semantics:
- Bind placeholders using the **enclosing mapping + inherited ancestor/global context** (string & scalar),
  with **enclosing keys taking precedence**.
- Resolve the token (qualified → direct; unqualified/partial → ladder).
- **Clone** the base object and **deep-merge** the field's own keys (field values win on conflicts).
- **Remove** ``base`` from the field value after merge.

Guardrails:
- **Self-inheritance** is **forbidden** (base cannot be the current object).
- **Descendant-inheritance** is **forbidden** (base cannot be inside the current object's subtree).
- **Cycles** in base chains are **forbidden** (detected by the resolver).

Fully-resolved base:
- When resolving ``base`` on concretes, the resolver clones the current **resolved** content of the base
  (from the canonical index), not its pre-expansion authored form.

``value from`` (general importer)
--------------------------------
A single meta key, ``value from``, lets authors copy values from elsewhere in the document into any authored slot.
It can be used both during template expansion (for parameters, locals, instance fields, and inline defaults) and
during resolution on any authored field of a concrete or template object.

**Syntax:**

.. code-block:: yaml

  # Import a parameter list (must be a sequence)
  template parameters: [x]
  x: { value from: "root.locals.detvar_names" }

  # Import a scalar constant
  settings:
    pot: { value from: "root.locals.constants.pot_run3" }

  # Import a mapping and refine it
  settings:
    value from: "root.locals.defaults.settings"
    log_level: debug   # deep-merge: authored keys win

  # Import a sequence from a sibling object during resolution
  universes: { value from: "root.runs.Run1.universes" }

**Semantics:**
- Resolve the **token** (qualified → direct; unqualified/partial → ladder) and **copy** the referenced node **in place**.
  The meta key ``value from`` is **pruned** from the final output.

- **Sub-field access.** Qualified and partial tokens that do not resolve directly to a canonical node
  are tried as a **prefix scope** plus **sub-field path**. For example, ``root.runs.Run1.universes``
  resolves the canonical node ``root.runs.Run1`` and navigates to its ``universes`` field.
  This works for any depth: ``root.runs.Run1.some.nested.field`` resolves through multiple levels.
  If an intermediate field is not a mapping, or a field does not exist, a clear error is raised.

- **Shape rules** at the usage site:
  - Under a parameter name listed in ``template parameters``, the imported value **must be a sequence**; otherwise Concretize error.
  - For scalar fields, the imported value **must be a scalar**; otherwise error.
  - For mapping fields, importing a mapping is allowed; if the field also has authored keys, implementations **deep-merge**
    the imported mapping with authored keys **winning** on conflicts.

- **Resolution** accepts both qualified tokens (e.g., ``root.locals.detvar_names``) and ladder (nearest-wins starting at the
  enclosing object scope). Authors SHOULD use **qualified tokens** for shared/global symbols to avoid shadowing.

- Sequence elements without ``id`` remain **unaddressable individually**; only the **sequence field** token is addressable.
  Elements with explicit ``id`` are registered as usual.

**Error examples:**
- Wrong shape:
  ``value from`` for parameter ``x`` must resolve to a sequence (got: scalar).
- Unknown token:
  ``value from`` could not resolve token ``locals.detvar_names`` (resolution: ladder).
- Missing sub-field:
  ``value from``: field ``universes`` not found in ``root.runs.Run1``.

Overrides
---------
Template-level overrides (broadcast)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Authored under a template as a mapping that applies to **every** instance (``overrides``).

**Application:** Template-level overrides are applied during the unified pass after expansion.
Mapping values deep-merge; scalars and sequences replace; explicit ``null`` clears.

Per-instance conditional overrides
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Authored as ``overrides per-instance``: a sequence of rules with ``when`` / ``replace``
(OR semantics within a key; last match wins).

**Application:** Conditional overrides are evaluated **per instance** using that instance's
parameters + locals context. The matching overlay is applied during the unified pass.

Object-level overrides (concretes)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An authored ``overrides`` mapping may appear on any concrete object.

**Application:** Object-level overrides are applied during the unified pass **after** object-level base merges
(so that fields inherited from a base exist before sequence-by-id targeting).

Sequence-by-id overrides
~~~~~~~~~~~~~~~~~~~~~~~~
When an override targets a **sequence field** with a **mapping** value, implementations interpret the mapping's
keys as **sequence element ids** and **deep-merge** the provided overlays into the element(s) whose ``id`` matches the key.

- **Strictness.** If any authored id does **not** match an element ``id`` in the target sequence, the resolver raises
  an error that lists the unknown id(s) and the path to the override. Unspecified elements are preserved. **Order is preserved.**
- **Elements lacking ``id``.** Sequence elements that do **not** declare ``id`` cannot be targeted by this override form.
  Overriding the field with a **sequence** value continues to **replace** the whole sequence.
- **Ordering inside unified pass.** Implementations apply:
  1) Template-level overrides,
  2) Per-instance conditional overrides,
  3) Object-level base merges,
  4) Concrete object-level overrides,
  5) Append directives.

Append
~~~~~~
When a concrete object or template inherits a sequence field (via ``base``, ``value from``, or direct authorship),
``append`` extends it with new elements without repeating the existing ones.

**Syntax (concrete object):**

.. code-block:: yaml

  - id: Run2
    universes: { value from: root.runs.Run1.universes }
    overrides:
      universes:
        CV: { recipe: { mc: { ... } } }
    append:
      universes:
        - id: NewUniverse
          recipe: [ ... ]

**Syntax (template):**

.. code-block:: yaml

  - id: "{x}"
    template parameters: [ x ]
    base: SomeBase
    append:
      universes:
        - id: "new_{{ x }}"
          recipe: [ ... ]

**Semantics:**
- ``append`` maps **sequence field names** to **sequences of new elements**.
- The target sequence must exist, be null, or not yet exist. If null or absent, a new sequence is created.
- If the target field exists but is not a sequence (e.g., a mapping or scalar), the resolver raises an error.
- The new elements are appended **after** all existing elements in the target sequence.
- ``append`` runs after template overrides, base merges, and concrete overrides, so overrides can modify
  inherited elements before new ones are added.
- On templates, ``append`` is captured during expansion, bound per-instance, and broadcast to all instances
  (analogous to template-level ``overrides``).
- Elements inside ``append`` may themselves be templates (``template parameters``), which are expanded
  during the concretize phase before the append is applied during resolution.



Binding (unified pass)
----------------------
- After structural resolution, all strings are bound using inherited context.
- Supports doubled-brace literals (``{{`` → ``{``, ``}}`` → ``}``).
- Unresolved placeholders after max passes cause an error.

Pruning
-------
- Drop **nulls** globally.
- Remove **meta** control sections like ``overrides``, ``value from``, and ``append``.
- Remove all **locals** sections and ``_``-prefixed auto-locals.
- Remove any fields listed in ``_locals_keys`` (locals originating in base or current template).
- Drop base provenance markers (e.g., ``_base_keys``) from the final resolved output **and** from the final in-memory DOM.

Identity persistence vs. local removal:
- Persist: authored **``id``**
- Prune: the entire ``locals`` mapping and all underscore auto-locals (including ``_id``).
  Meta keys, base provenance markers, and explicit ``null`` values are also pruned.

Error handling
--------------
- YAML anchors and aliases are **not allowed** (both preflight in raw text and in the DOM).
- Duplicate tokens within a scope cause an error.
- Ambiguous unqualified/partial references cause an error.
- Cycles in base chains cause an error.
- Explicit ``id`` present inside a **mapping section child** causes an error.
- Parameter length mismatch, malformed conditional override rules, or unknown base references cause errors.
- Shape mismatches at ``value from`` usage sites cause errors (e.g., importing a scalar into a parameter list).
  Unknown tokens include a resolution hint (``qualified`` / ``ladder``).

Ambiguity and ladder errors
---------------------------
- Ambiguity is detected **per rung** of the parent-chain ladder (current scope, parent, grandparent, …).
- If a rung yields **more than one** match:
  - For **unqualified tokens**: the reference is **ambiguous**, resolution fails listing **only** the matches from that rung.
  - For **partially-scoped tokens**: if a rung yields **more than one complete chain match**, resolution fails listing **only**
    the matches from that rung.
- If **no rung** yields a match, resolution fails with a **ladder** resolution hint.

Final emission
--------------
Implementations SHOULD omit:
- All **meta** control sections like ``overrides``, ``value from``, and ``append``,
- All **locals** sections and ``_``-prefixed auto-locals,
- Any field with explicit **``null``** value,
- Base provenance markers (``_base_keys``),

from the final output **and** from the final in-memory DOM.

Error path formatting
---------------------
Error paths SHOULD include **all sequence indexes** in their segments for clarity.

Example error::

  root.runs[0].files[2]/template: Unqualified reference 'numu' not found via ladder for scope 'root.runs.files'.

Error Path Semantics
--------------------
Error paths remain **index-based** for clarity, even when identity tokens exist:

- For sequences **without explicit `id`**, use `[i]` notation (e.g., `root.runs[0].universes[3]`).
- For sequences **with explicit `id`**, use the `id` token in canonical identity for scope construction, but **do not** replace indexes in error paths.

**Rationale:**
- Index-based paths are stable and unambiguous for debugging.
- Identity tokens are reserved for lookup and inheritance logic, not for error reporting.

**Recommendation:**
Implementations SHOULD:
- Always include numeric indexes in error paths.
- Optionally append identity tokens in brackets for diagnostics (e.g., `root.runs0.universes3`).

Resolution order (implementation summary)
-----------------------------------------
1. **Preflight Anchors/Aliases:** Reject anchors/aliases in raw text.
2. **DOM Anchors/Aliases:** Reject anchors/aliases in the parsed DOM.
3. **Collection & Indexing:** Traverse; for each mapping section register **all child keys** as tokens under the section scope.
   For sequence sections, register elements that declare explicit ``id``. Duplicate tokens inside a scope are errors.
4. **Template Expansion (Concretize):**
   - Bind placeholders in **object-level ``base``** (using parameters), resolve base, clone base;
     **mark base-derived keys** and update dynamic provenance as template material writes fields.
   - **Locals** (parameters-only context; intra-locals chaining).
   - **Instance fields** (per-index; parameters-only context).
   - **Inline fields** (defaults; set only if missing, null, or from base per provenance).
   - **Select/register identities** (id → _id) as available.
5. **Unified Pass:**
   - **Resolve ``value from`` fields** (including sub-field access) at the start of walking each mapping.
   - **Template-level overrides** → **Per-instance conditional overrides**.
   - **Object-level base** merges (guardrails; clone final base and deep-merge).
   - **Concrete object-level overrides**, including **sequence-by-id** targeting (strict unknown-id checks; order preserved).
   - **Append** (extend sequences with new elements from ``append`` directives).
   - **Global binding** using inherited context (root + ancestors + current mapping).
   - **Prune** locals/meta/auto-locals and **nulls** everywhere; drop any keys listed in ``_locals_keys`` and
     drop base provenance markers.
6. **Emission:** Serialize with ordered mappings; locals/meta/markers removed; nulls removed.

.. _no-anchors-aliases:

Why YAML anchors/aliases are forbidden in YODEL
----------------------------------------------
YODEL forbids YAML anchors (``&``) and aliases (``*``) at both preflight and DOM levels. This is intentional:

* **Provenance clarity.** Anchors/aliases create implicit cross-links; YODEL emphasizes explicit identity scoping
  and token-based resolution. Implicit sharing obscures “where a value came from” and makes overrides harder to reason about.

* **Resolver semantics.** YODEL indexes identities per scope (e.g., ``root.section.id``) and applies nearest-wins binding
  with guardrails. Aliases can inject content without scope/identity context, undermining those rules.

* **Mutation illusions.** Many YAML toolchains treat aliases as references. YODEL operates with immutable clones and explicit
  deep-merge semantics; shared references contradict that model and often cause subtle bugs.

* **Security and portability.** Anchors permit terse but opaque graph wiring that depends on parser-specific behaviors.
  Keeping them off ensures all dependencies are explicitly named (via identity tokens or ``value from``) and validated.

If you need reuse across templates, prefer explicit mechanisms:
* **Template base inheritance** (for structured mappings),
* **``value from``** (to import arrays/scalars/mappings by token),
* **Named ``locals``** (for simple scalars/sequences intended for reuse).

.. _binding-context-rationale:

Rationale: Single-phase binding
-------------------------------

YODEL binds string placeholders in a single phase during template expansion,
using **parameters and locals only** (including intra-locals chaining). This
approach avoids the complexity of two-phase binding while remaining sufficient
for all real-world use cases:

* **Parameters and locals** are fully determined at expansion time. The template's
  parameter arrays and authored locals provide all the names needed to bind
  placeholders in strings like ``{dir}/stv-{_fname}``.
* **Inherited context binding is unnecessary** because the examples that motivated
  the original two-phase design all express their dependencies via explicit
  parameters, ``value from``, or base inheritance rather than relying on
  nearest-wins ancestor shadowing.

By binding during expansion, YODEL eliminates the complexity of a second binding
phase while preserving the same expressive power for all documented configurations.
Doubled-brace literals (``{{`` and ``}}``) remain protected during binding.
