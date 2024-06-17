bool amIRelaxed=true;
int inloop=0;

#include "gdlLexer.h"
#include "gdlParser.h"

using namespace antlr4;

int main(int , const char **) {

  ANTLRInputStream * input = new ANTLRInputStream(std::cin); // read stdin
  gdlLexer *lexer = new gdlLexer(input);
  CommonTokenStream *tokens = new CommonTokenStream(lexer);

  //  tokens->fill();
//  for (auto token : tokens->getTokens()) {
//        std::cout << token->toString() << std::endl;
//  }
//
  gdlParser * parser=new gdlParser(tokens);
  tree::ParseTree * tree = parser->translation_unit();

  //  std::cout << tree->toStringTree(&parser) << std::endl << std::endl;

  return 0;
}
