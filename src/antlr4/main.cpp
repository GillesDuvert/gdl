bool amIRelaxed=true;
int inloop=0;

#include "gdlLexer.h"
#include "gdlParser.h"

using namespace antlr4;
using namespace std;

int main(int argc, char *argv[]){
    bool showtree=false;
    bool showtokens=false;
    bool token_only=false;
    for( auto a=1; a< argc; ++a)
    {
      if( string( argv[a]) == "--help" || string( argv[a]) == "-h") {
      cerr << "Usage: cat xxx.pro | demo [-options]" << endl;
      cerr << "options:" << endl;
      cerr << "  --help (-h)        display this message" << endl;
      cerr << "  --tree (-t)    show tree" << endl;
      cerr << "  --token-only (-x)  compute only tokens (speed test for lexer)" << endl;
      cerr << "  --tokens (-o)    show tokens" << endl;
      return 0;
      }
      if (string(argv[a])=="--tree" || string(argv[a])=="-t" || string(argv[a])=="-T")
	{
	  showtree=true;
	}
      if (string(argv[a])=="--tokens" || string(argv[a])=="-o" || string(argv[a])=="-O")
	{
	  showtokens=true;
	} 
      if (string(argv[a])=="--token-only" || string(argv[a])=="-x" || string(argv[a])=="-X")
	{
	  token_only=true;
	} 
   }
    
  ANTLRInputStream * input = new ANTLRInputStream(std::cin); // read stdin
  gdlLexer *lexer = new gdlLexer(input);
  CommonTokenStream *tokens = new CommonTokenStream(lexer);
  if (token_only) {
    tokens->fill();
    if (showtokens) {
      for (auto token : tokens->getTokens()) std::cout << token->toString() << std::endl;
    }
    return 0;
  }
  
  gdlParser * parser=new gdlParser(tokens);
  tree::ParseTree * tree = parser->translation_unit();

  if (showtokens) {
    for (auto token : tokens->getTokens()) std::cout << token->toString() << std::endl;
  }
  if (showtree) std::cout << tree->toStringTree(&parser) << std::endl << std::endl;

  return 0;
}
