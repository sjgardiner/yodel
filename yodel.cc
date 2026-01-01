#include "yodel.hh"

int main() {
  try {
    yodel::Resolver yodel;
    yodel::ordered_node resolved = yodel.resolve( std::cin );
    std::cout << yodel::ordered_node::serialize( resolved );
    return 0;
  } catch (const std::exception& ex) {
    std::cerr << "[yodel] error: " << ex.what() << "\n";
    return 1;
  }
}
