#include "meta.h"
#include "objects.h"

struct ObjectBuilder {
  using BuildMethod = ObjPtr (*)(ObjectBuilder &, std::string &name,
                                 SyntaxObject *);

  ObjectBuilder();

  ObjPtr build(SyntaxObject &syntax);

  std::unordered_map<std::string, BuildMethod> builders_;
};

std::vector<std::string> parseArgList(SyntaxObject *syntax);

std::vector<ObjPtr> lispListToVec(ObjectBuilder &builder, SyntaxObject *syntax);

ObjPtr codeWalk(ObjectBuilder &builder, SyntaxObject &syntax);

ObjPtr ir1LispTransform(std::unique_ptr<SyntaxObject> syntax);