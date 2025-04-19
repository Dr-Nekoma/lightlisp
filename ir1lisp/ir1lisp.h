#include "../definitions/interfaces.h"

struct ObjectBuilder {
  using BuildMethod = ObjPtr (*)(ObjectBuilder &, std::string &name,
                                 SyntaxObject &);

  ObjectBuilder();

  ObjPtr build(SyntaxObject &syntax);

  std::unordered_map<std::string, BuildMethod> builders_;
};

std::vector<std::string> parseArgList(SyntaxObject &);

std::vector<ObjPtr> lispListToVec(SyntaxObject &);

ObjPtr codeWalk(ObjectBuilder &builder, SyntaxObject &syntax);

ObjPtr ir1LispTransform(std::shared_ptr<SyntaxObject> syntax);