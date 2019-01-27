/*
 * Copyright 2014 Google Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// independent from idl_parser, since this code is not needed for most clients

#include <algorithm>
#include <iostream>
#include <iterator>
#include <map>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include "flatbuffers/code_generators.h"
#include "flatbuffers/flatbuffers.h"
#include "flatbuffers/idl.h"
#include "flatbuffers/util.h"

namespace flatbuffers {
namespace ocaml {

// Hardcode spaces per indentation.
const std::string Indent = "    ";

#if 0
const std::string kGeneratedFileNamePostfix = "_generated";

static std::string GeneratedFileName(const std::string &path,
                                     const std::string &file_name) {
  return path + file_name + kGeneratedFileNamePostfix + ".ml";
}
#endif

typedef std::unordered_set<std::string> StringSet;

class Struct {
 public:
  const flatbuffers::Namespace *ns;
  std::string name;
  std::string code;
  StringSet dependencies;
  Struct() {}
  Struct(const flatbuffers::Namespace *_ns, const std::string &_name,
         const std::string &_code, const StringSet &_dependencies)
      : ns(_ns), name(_name), code(_code), dependencies(_dependencies) {}
};

class Namespace {
 private:
  std::string name;
  std::unordered_map<std::string, Struct> structs;
  std::map<std::string, Namespace> namespaces;

  Namespace *resolve_one(const std::string &ns) {
    if (this->namespaces.count(ns) == 0) {
      Namespace empty;
      empty.name = ns;
      this->namespaces.insert(std::pair<std::string, Namespace>(ns, empty));
    }
    auto it = this->namespaces.find(ns);
    return &it->second;
  }

  Namespace &resolve(const flatbuffers::Namespace *ns) {
    Namespace *current = this;
    if (ns) {
      for (auto it = ns->components.begin(); it != ns->components.end(); it++) {
        current = current->resolve_one(*it);
      }
    }
    return *current;
  }

  static bool setContains(const StringSet &a, const StringSet &b) {
    for (auto it = b.begin(); it != b.end(); it++) {
      if (a.count(*it) == 0) { return false; }
    }
    return true;
  }

  void printStucts(std::string *code_ptr, StringSet *known) {
    StringSet printed;
    bool skip_dependencies = false;
    for (;;) {
      bool advanced = false;
      StringSet skipped;
      for (auto it = structs.begin(); it != structs.end(); it++) {
        const Struct &s = it->second;
        if (printed.count(s.name)) { continue; }
        if (skip_dependencies || setContains(*known, s.dependencies)) {
          advanced = true;
          if (skip_dependencies) {
            std::cerr << "Module: " << s.name << std::endl;
            std::cerr << "Dependencies: " << std::endl;
            for (auto d = s.dependencies.begin(); d != s.dependencies.end();
                 d++) {
              auto n2 = *d;
              if (printed.count(n2) == 0) {
                std::cerr << n2 << '(';
                auto s2 = structs[n2];
                for (auto d2 = s2.dependencies.begin();
                     d2 != s2.dependencies.end(); d2++) {
                  auto n3 = *d2;
                  if (printed.count(n3) == 0) { std::cerr << n3 << ' '; }
                }
                std::cerr << ") ";
              }
            }
            std::cerr << std::endl << std::endl;
            if (0) FLATBUFFERS_ASSERT(0);
          }
          printed.insert(s.name);
          known->insert(s.name);
          *code_ptr += s.code;
        } else {
          skipped.insert(s.name);
        }
      }
      if (skipped.empty()) { break; }
      if (!advanced) { skip_dependencies = true; }
    }
  }

  void print_rec(std::string *code_ptr, StringSet *known_structures) {
    if (!this->name.empty()) {
      *code_ptr += "module " + this->name + " = struct\n\n";
    }

    printStucts(code_ptr, known_structures);
    for (auto it = namespaces.begin(); it != namespaces.end(); it++) {
      it->second.print_rec(code_ptr, known_structures);
    }
    if (!this->name.empty()) { *code_ptr += "end (* " + this->name + " *)\n"; }
  }

 public:
  Namespace() {}
  void addStruct(const flatbuffers::Namespace *ns, const std::string &_name,
                 const StringSet &dependencies, const std::string &code) {
    Namespace &target = resolve(ns);
    Struct _struct(ns, _name, code, dependencies);
    target.structs[_name] = _struct;
    return;
  }
  void print(std::string *code_ptr) {
    StringSet known;
    print_rec(code_ptr, &known);
  }
};

class OcamlGenerator : public BaseGenerator {
 protected:
  Namespace ns;

 public:
  OcamlGenerator(const Parser &parser, const std::string &path,
                 const std::string &file_name)
      : BaseGenerator(parser, path, file_name, "" /* not used */,
                      "" /* not used */, "ml"),
        float_const_gen_("Float.nan", "Float.infinity", "Float.neg_infinity") {
    static const char *const keywords[] = {
      "and",        "as",       "assert", "asr",     "begin",   "class",
      "constraint", "do",       "done",   "downto",  "else",    "end",
      "exception",  "external", "false",  "for",     "fun",     "function",
      "functor",    "if",       "in",     "include", "inherit", "initializer",
      "land",       "lazy",     "let",    "lor",     "lsl",     "lsr",
      "lxor",       "match",    "method", "mod",     "module",  "mutable",
      "new",        "nonrec",   "object", "of",      "open",    "or",
      "private",    "rec",      "sig",    "struct",  "then",    "to",
      "true",       "try",      "type",   "val",     "virtual", "when",
      "while",      "with"
    };
    keywords_.insert(std::begin(keywords), std::end(keywords));
  }

  void BeginModule(const std::string class_name, std::string *code_ptr) {
    *code_ptr += "module " + class_name + " = struct\n";
  }

  const char *GetScalarType(const StructDef &struct_def, const Type &type) {
    (void)struct_def;
    switch (type.base_type) {
      case BASE_TYPE_UINT: return "Int64.t";
      case BASE_TYPE_ULONG: return "Int64.t";
      case BASE_TYPE_LONG: return "Int64.t";
      case BASE_TYPE_INT: return "Int32.t";
      case BASE_TYPE_DOUBLE: return "float";
      case BASE_TYPE_FLOAT: return "float";
      case BASE_TYPE_BOOL: return "bool";
      default: return "int";
    }
  }

  std::string GetStructType(const StructDef &struct_def,
                            const FieldDef &field) {
    if (struct_def.name.compare(TypeName(field)) == 0) {
      return "t";
    } else {
      return MakeCamel(TypeName(field)) + ".t";
    }
  }

  void GenerateStructType(const StructDef &struct_def, StringSet *dependencies,
                          std::string *code_ptr) {
    *code_ptr += Indent + "type t";
    if (struct_def.fields.vec.empty()) {
      *code_ptr += "\n\n";
      return;
    }
    *code_ptr += " = {\n";
    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (field.deprecated) continue;
      if (field.value.type.base_type != BASE_TYPE_UNION &&
          (field.value.type.enum_def && field.value.type.enum_def->is_union)) {
        continue;
      }

      const auto &name = NormalizedName(field);
      *code_ptr += Indent + Indent + name + " : ";

      if (IsScalar(field.value.type.base_type)) {
        if (field.value.type.enum_def) {
          *code_ptr += NormalizedName(*field.value.type.enum_def) + ".t";
        } else {
          *code_ptr += GetScalarType(struct_def, field.value.type);
        }
      } else {
        switch (field.value.type.base_type) {
          case BASE_TYPE_STRUCT:
            *code_ptr += GetStructType(struct_def, field);
            if (!struct_def.fixed) { *code_ptr += " option"; }
            break;
          case BASE_TYPE_STRING: *code_ptr += "string option"; break;
          case BASE_TYPE_VECTOR: {
            auto vectortype = field.value.type.VectorType();
            if (vectortype.base_type == BASE_TYPE_STRUCT) {
              *code_ptr += GetScalarType(struct_def, field.value.type);
            } else if (IsScalar(vectortype.base_type)) {
              *code_ptr += GetScalarType(struct_def, vectortype);
            } else {
              *code_ptr += GetStructType(struct_def, field);
            }
            break;
          }
          case BASE_TYPE_UNION:
            dependencies->insert(NormalizedName(field.value.type));
            *code_ptr += getUnionName(field.value.type) + ".t";
            break;
          default: FLATBUFFERS_ASSERT(0); break;
        }
      }
      *code_ptr += ";\n";
    }
    *code_ptr += Indent + "}\n";
  }

  void BeginStruct(const StructDef &struct_def, StringSet *dependencies,
                   std::string *code_ptr) {
    BeginModule(NormalizedName(struct_def), code_ptr);

    if (parser_.opts.generate_object_based_api) {
      GenerateStructType(struct_def, dependencies, code_ptr);
    } else {
      *code_ptr += Indent + "type t\n\n";
    }
    *code_ptr += Indent + "type offset = t ByteBuffer.offset\n\n";
    *code_ptr +=
        Indent + "let init b pos : offset = ByteBuffer.offset b pos\n\n";
    *code_ptr +=
        Indent + "let of_union o : offset = ByteBuffer.offset_of_union o\n\n";
  }

  void BeginEnum(const std::string class_name, std::string *code_ptr) {
    BeginModule(class_name, code_ptr);
  }

  std::string EscapeKeyword(const std::string &name) const {
    return keywords_.find(name) == keywords_.end() ? name : name + "_";
  }

  std::string NormalizedName(const Definition &definition) const {
    return EscapeKeyword(definition.name);
  }

  std::string NormalizedName(const EnumVal &ev) const {
    return EscapeKeyword(ev.name);
  }

  std::string GenTypeBasic(const Type &type) const {
    static const char *ctypename[] = {
    // clang-format off
      #define FLATBUFFERS_TD(ENUM, IDLTYPE, \
              CTYPE, JTYPE, GTYPE, NTYPE, PTYPE, ...) \
        #PTYPE,
        FLATBUFFERS_GEN_TYPES(FLATBUFFERS_TD)
      #undef FLATBUFFERS_TD
      // clang-format on
    };
    return ctypename[type.base_type];
  }

  std::string NormalizedName(const Type &type) const {
    if (type.struct_def) { return NormalizedName(*(type.struct_def)); }
    if (type.enum_def) { return NormalizedName(*(type.enum_def)); }
    return GenTypeBasic(type);
  }

  // A single enum member.
  void EnumMember(const EnumVal ev, std::string &type, std::string &of_int,
                  std::string &to_int) {
    std::string name = NormalizedName(ev);
    std::string value = NumToString(ev.GetAsInt64());
    type += Indent + Indent + "| " + name + "\n";
    // code += NumToString(ev.value) + "\n";
    of_int += Indent + Indent + "| " + value + " -> " + name + "\n";
    to_int += Indent + Indent + "| " + name + " -> " + value + "\n";
    return;
  }

  void EndModule(std::string *code_ptr) { *code_ptr += "end\n\n"; }

  void EndEnum(std::string *code_ptr) { EndModule(code_ptr); }

  void EndStruct(std::string *code_ptr) { EndModule(code_ptr); }

  // Initialize a new struct or table from existing data.
  void NewRootTypeFromBuffer(const StructDef &struct_def,
                             std::string *code_ptr) {
    std::string &code = *code_ptr;

    code += Indent + "let getRootAs" + NormalizedName(struct_def) + " b =\n";
    code += Indent + Indent + "let offset = ";
    code +=
        "(ByteBuffer.read_ocaml_int32 b (ByteBuffer.position b)) + "
        "(ByteBuffer.position b) in\n";
    code += Indent + Indent + "init b offset\n\n";
  }

  std::string GetScalarAccessorType(const StructDef &struct_def,
                                    const Type &type) {
    (void)struct_def;
    return MakeCamel(GenTypeBasic(type));
  }

  std::string GetScalarReceiver(const StructDef &struct_def, const Type &type) {
    std::string code;
    code += "ByteBuffer.";
    code += "read" + GetScalarAccessorType(struct_def, type) +
            " t.ByteBuffer.b offset";
    return code;
  }

  const char *GetScalarZero(const StructDef &struct_def, const Type &type) {
    (void)struct_def;
    switch (type.base_type) {
      case BASE_TYPE_UINT:
      case BASE_TYPE_ULONG:
      case BASE_TYPE_LONG: return "Int64.zero";
      case BASE_TYPE_INT: return "Int32.zero";
      case BASE_TYPE_DOUBLE:
      case BASE_TYPE_FLOAT: return "0.0";
      case BASE_TYPE_BOOL: return "false";
      default: return "0";
    }
  }

  std::string GetScalarConstant(const StructDef &struct_def, const Type &type,
                                const std::string &value) {
    if (value.compare("0") == 0) { return GetScalarZero(struct_def, type); }
    switch (type.base_type) {
      case BASE_TYPE_UINT:
      case BASE_TYPE_ULONG:
      case BASE_TYPE_LONG: return value + "L";
      case BASE_TYPE_INT: return value + "l";
      default: return value;
    }
  }

  std::string GetStructReceiver(const StructDef &struct_def,
                                const FieldDef &field,
                                StringSet *dependencies) {
    std::string code;
    if (struct_def.name.compare(TypeName(field)) == 0) {
      code += "init";
    } else {
      std::string module = MakeCamel(TypeName(field));
      dependencies->insert(module);
      code += module + ".init";
    }
    code += " t.ByteBuffer.b offset";
    return code;
  }

  void GenReceiver(const StructDef &struct_def, const FieldDef &field,
                   std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += Indent + "(* " + NormalizedName(struct_def) + " *)\n";
    code += Indent + "let ";
    code += NormalizedName(field);
    code += " (t:offset) =\n";
  }

  std::string GetRelativeOffset(const std::string &offset) {
    return "let offset = t.ByteBuffer.pos + " + offset + " in";
  }

  // Get the value of a struct's scalar.
  void GetScalarFieldOfStruct(const StructDef &struct_def,
                              const FieldDef &field, std::string *code_ptr) {
    GenReceiver(struct_def, field, code_ptr);
    std::string &code = *code_ptr;
    code += Indent + Indent +
            GetRelativeOffset(NumToString(field.value.offset)) + "\n";
    code += Indent + Indent + GetScalarReceiver(struct_def, field.value.type) +
            "\n";
  }

  // Get the value of a table's scalar.
  void GetScalarFieldOfTable(const StructDef &struct_def, const FieldDef &field,
                             std::string *code_ptr) {
    GenReceiver(struct_def, field, code_ptr);
    std::string &code = *code_ptr;
    code +=
        Indent + Indent +
        "let offset = ByteBuffer.__offset t.ByteBuffer.b t.ByteBuffer.pos " +
        NumToString(field.value.offset) + " in\n";
    std::string default_value;
    auto is_bool = IsBool(field.value.type.base_type);
    if (is_bool) {
      default_value = field.value.constant == "0" ? "false" : "true";
    } else {
      default_value = IsFloat(field.value.type.base_type)
                          ? float_const_gen_.GenFloatConstant(field)
                          : GetScalarConstant(struct_def, field.value.type,
                                              field.value.constant);
    }
    std::string field_value;
    field_value = GetRelativeOffset("offset") + " " +
                  GetScalarReceiver(struct_def, field.value.type);
    if (field.value.type.enum_def) {
      auto module_name = NormalizedName(*field.value.type.enum_def);
      field_value = module_name + ".of_int (" + field_value + ")";
      if (auto val = field.value.type.enum_def->ReverseLookup(
              StringToInt(field.value.constant.c_str()), false)) {
        default_value = module_name + "." + val->name;
      }
    }
    code += Indent + Indent + "if(offset!=0) then " + field_value + "\n";
    code += Indent + Indent + "else " + default_value + "\n";
  }

  // Get a struct by initializing an existing struct.
  // Specific to Struct.
  void GetStructFieldOfStruct(const StructDef &struct_def,
                              const FieldDef &field, std::string *code_ptr,
                              StringSet *dependencies) {
    GenReceiver(struct_def, field, code_ptr);
    std::string &code = *code_ptr;
    code += Indent + Indent +
            GetRelativeOffset(NumToString(field.value.offset)) + "\n";
    code += Indent + Indent +
            GetStructReceiver(struct_def, field, dependencies) + "\n";
  }

  // Get a struct by initializing an existing struct.
  // Specific to Table.
  void GetStructFieldOfTable(const StructDef &struct_def, const FieldDef &field,
                             std::string *code_ptr, StringSet *dependencies) {
    GenReceiver(struct_def, field, code_ptr);
    std::string &code = *code_ptr;
    code += Indent + Indent + "let offset = ";
    if (field.value.type.struct_def->fixed) {
      code += "ByteBuffer.__offset t.ByteBuffer.b t.ByteBuffer.pos " +
              NumToString(field.value.offset);
    } else {
      code += "t.ByteBuffer.pos + (ByteBuffer.__indirect t.ByteBuffer.b " +
              NumToString(field.value.offset) + ")";
    }
    code += " in\n";
    code += Indent + Indent + "if(offset!=0) then Some (";
    code += GetRelativeOffset("offset") + " " +
            GetStructReceiver(struct_def, field, dependencies) + ")\n";
    code += Indent + Indent + "else None\n";
  }

  // Get the value of a string.
  void GetStringField(const StructDef &struct_def, const FieldDef &field,
                      std::string *code_ptr) {
    GenReceiver(struct_def, field, code_ptr);
    std::string &code = *code_ptr;
    code +=
        Indent + Indent +
        "let offset = ByteBuffer.__offset t.ByteBuffer.b t.ByteBuffer.pos " +
        NumToString(field.value.offset) + " in\n";
    code += Indent + Indent +
            "if(offset!=0) then Some (ByteBuffer.__string t.ByteBuffer.b "
            "(t.ByteBuffer.pos + offset))\n";
    code += Indent + Indent + "else None\n";
  }

  // Get the value of a union from an object.
  void GetUnionField(const StructDef &struct_def, const FieldDef &field,
                     std::string *code_ptr) {
    std::string &code = *code_ptr;
    GenReceiver(struct_def, field, code_ptr);
    code += Indent + Indent + "ByteBuffer.__union t " +
            NumToString(field.value.offset) + "\n";
  }

  void GenMemberOfVectorCommon(const FieldDef &field, std::string *code_ptr) {
    std::string &code = *code_ptr;
    std::string offset =
        Indent + Indent +
        "let offset = ByteBuffer.__offset t.ByteBuffer.b t.ByteBuffer.pos " +
        NumToString(field.value.offset) + " in\n";
    code += Indent + "let " + NormalizedName(field) + "Length (t:offset) =\n";
    code += offset;
    code += Indent + Indent +
            "if(offset!=0) then ByteBuffer.__vector_len t.ByteBuffer.b "
            "(t.ByteBuffer.pos + offset)\n";
    code += Indent + Indent + "else 0\n\n";
    code += Indent + "let " + NormalizedName(field) + " (t:offset) index =\n";
    code += offset;
    code += Indent + Indent + "if(offset!=0) then\n";
    code += Indent + Indent + Indent + "let index = index";
    auto vectortype = field.value.type.VectorType();
    auto inline_size = InlineSize(vectortype);
    if (inline_size != 1) { code += " * " + NumToString(inline_size); }
    code += " in\n";
    code += Indent + Indent + Indent +
            "let offset = (ByteBuffer.__vector t.ByteBuffer.b "
            "(t.ByteBuffer.pos + offset)) + index in\n";
  }

  // Get the value of a vector's struct member.
  void GetMemberOfVectorOfStruct(const StructDef &struct_def,
                                 const FieldDef &field, std::string *code_ptr,
                                 StringSet *dependencies) {
    std::string &code = *code_ptr;
    GenMemberOfVectorCommon(field, code_ptr);
    code += Indent + Indent + Indent +
            "let offset = ByteBuffer.__indirect t.ByteBuffer.b offset in\n";
    code += Indent + Indent + Indent + "Some (" +
            GetStructReceiver(struct_def, field, dependencies) + ")\n";
    code += Indent + Indent + "else None\n";
  }

  // Get the value of a vector's non-struct member. Uses a named return
  // argument to conveniently set the zero value for the result.
  void GetMemberOfVectorOfNonStruct(const StructDef &struct_def,
                                    const FieldDef &field,
                                    std::string *code_ptr) {
    std::string &code = *code_ptr;
    auto vectortype = field.value.type.VectorType();

    GenMemberOfVectorCommon(field, code_ptr);
    if (IsScalar(vectortype.base_type)) {
      code += Indent + Indent + Indent +
              GetScalarReceiver(struct_def, vectortype) + "\n";
      code += Indent + Indent + "else " +
              GetScalarZero(struct_def, vectortype) + "\n";
    } else if (vectortype.base_type == BASE_TYPE_STRING) {
      code += Indent + Indent + Indent +
              "Some (ByteBuffer.__string t.ByteBuffer.b offset)\n";
      code += Indent + Indent + "else None\n";
    } else {
      FLATBUFFERS_ASSERT(0);
    }
  }

  // Begin the creator function signature.
  void BeginBuilderArgs(const StructDef &struct_def, std::string *code_ptr) {
    std::string &code = *code_ptr;

    code += "\n";
    code += Indent + "let create" + NormalizedName(struct_def);
    code += " builder";
  }

  // Recursively generate arguments for a constructor, to deal with nested
  // structs.
  void StructBuilderArgs(const StructDef &struct_def, const char *nameprefix,
                         std::string *code_ptr) {
    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (IsStruct(field.value.type)) {
        // Generate arguments for a struct inside a struct. To ensure names
        // don't clash, and to make it obvious these arguments are constructing
        // a nested struct, prefix the name with the field name.
        StructBuilderArgs(*field.value.type.struct_def,
                          (nameprefix + (NormalizedName(field) + "_")).c_str(),
                          code_ptr);
      } else {
        std::string &code = *code_ptr;
        code += std::string(" ") + "~" + nameprefix;
        code += MakeCamel(NormalizedName(field), false);
      }
    }
  }

  // End the creator function signature.
  void EndBuilderArgs(std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += " =\n";
  }

  // Recursively generate struct construction statements and instert manual
  // padding.
  void StructBuilderBody(const StructDef &struct_def, const char *nameprefix,
                         std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += Indent + Indent + "Builder.prep builder ~additional_bytes:" +
            NumToString(struct_def.minalign) + " ";
    code += NumToString(struct_def.bytesize) + ";\n";
    for (auto it = struct_def.fields.vec.rbegin();
         it != struct_def.fields.vec.rend(); ++it) {
      auto &field = **it;
      if (field.padding)
        code += Indent + Indent + "Builder.pad builder " +
                NumToString(field.padding) + ";\n";
      if (IsStruct(field.value.type)) {
        StructBuilderBody(*field.value.type.struct_def,
                          (nameprefix + (NormalizedName(field) + "_")).c_str(),
                          code_ptr);
      } else {
        code +=
            Indent + Indent + "Builder.add" + GenMethod(field) + " builder ";
        code += nameprefix + MakeCamel(NormalizedName(field), false) + ";\n";
      }
    }
  }

  void EndBuilderBody(std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += Indent + Indent + "Builder.offset builder\n";
  }

  // Get the value of a table's starting offset.
  void GetStartOfTable(const StructDef &struct_def, std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += Indent + "let start" + NormalizedName(struct_def);
    code += " builder =\n";
    code += Indent + Indent + "Builder.startObject  builder ";
    code += NumToString(struct_def.fields.vec.size());
    code += "\n\n";
  }

  // Set the value of a table's field.
  void BuildFieldOfTable(const StructDef &struct_def, const FieldDef &field,
                         const size_t offset, std::string *code_ptr) {
    std::string &code = *code_ptr;
    std::string name = MakeCamel(NormalizedName(field), false);
    auto type = field.value.type;
    code += Indent + "let add" + MakeCamel(NormalizedName(field));
    code += " builder " + name + " =\n";
    code += Indent + Indent + "Builder.addField";
    if (IsScalar(type.base_type)) {
      code += GetScalarAccessorType(struct_def, type);
    } else {
      code += "Offset";
    }
    code += " builder " + NumToString(offset) + " " + name;
    code += " ";
    auto default_value = IsFloat(field.value.type.base_type)
                             ? float_const_gen_.GenFloatConstant(field)
                             : GetScalarConstant(struct_def, field.value.type,
                                                 field.value.constant);

    code += '(' + default_value + ')';
    code += "\n\n";
  }

  // Set the value of one of the members of a table's vector.
  void BuildVectorOfTable(const StructDef &struct_def, const FieldDef &field,
                          std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += Indent + "let start" + NormalizedName(struct_def) +
            MakeCamel(NormalizedName(field)) + " builder numElems =\n";
    auto vector_type = field.value.type.VectorType();
    auto elem_size = InlineSize(vector_type);
    auto alignment = InlineAlignment(vector_type);
    code += Indent + Indent + "Builder.startVector builder ";
    code += NumToString(elem_size);
    code += " numElems " + NumToString(alignment);
    code += "\n\n";
  }

  // Get the offset of the end of a table.
  void GetEndOffsetOnTable(const StructDef &struct_def, std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += Indent + "let end" + NormalizedName(struct_def);
    code += " builder =\n";
    code += Indent + Indent + "Builder.endObject builder\n\n";
  }

  // Generate a struct field, conditioned on its child type(s).
  void GenStructAccessor(const StructDef &struct_def, const FieldDef &field,
                         std::string *code_ptr, StringSet *dependencies) {
    GenComment(field.doc_comment, code_ptr);
    if (IsScalar(field.value.type.base_type)) {
      if (struct_def.fixed) {
        GetScalarFieldOfStruct(struct_def, field, code_ptr);
      } else {
        GetScalarFieldOfTable(struct_def, field, code_ptr);
      }
    } else {
      switch (field.value.type.base_type) {
        case BASE_TYPE_STRUCT:
          if (struct_def.fixed) {
            GetStructFieldOfStruct(struct_def, field, code_ptr, dependencies);
          } else {
            GetStructFieldOfTable(struct_def, field, code_ptr, dependencies);
          }
          break;
        case BASE_TYPE_STRING:
          GetStringField(struct_def, field, code_ptr);
          break;
        case BASE_TYPE_VECTOR: {
          auto vectortype = field.value.type.VectorType();
          if (vectortype.base_type == BASE_TYPE_STRUCT) {
            GetMemberOfVectorOfStruct(struct_def, field, code_ptr,
                                      dependencies);
          } else {
            GetMemberOfVectorOfNonStruct(struct_def, field, code_ptr);
          }
          break;
        }
        case BASE_TYPE_UNION: GetUnionField(struct_def, field, code_ptr); break;
        default: FLATBUFFERS_ASSERT(0); break;
      }
    }
    *code_ptr += "\n";
  }

  // Generate table constructors, conditioned on its members' types.
  void GenTableBuilders(const StructDef &struct_def, std::string *code_ptr) {
    GetStartOfTable(struct_def, code_ptr);

    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (field.deprecated) continue;

      auto offset = it - struct_def.fields.vec.begin();
      BuildFieldOfTable(struct_def, field, offset, code_ptr);
      if (field.value.type.base_type == BASE_TYPE_VECTOR) {
        BuildVectorOfTable(struct_def, field, code_ptr);
      }
    }

    GetEndOffsetOnTable(struct_def, code_ptr);
  }

  // Generate struct or table methods.
  void GenStruct(const StructDef &struct_def) {
    if (struct_def.generated) return;
    std::string code;
    StringSet dependencies;

    GenComment(struct_def.doc_comment, &code);
    BeginStruct(struct_def, &dependencies, &code);
    if (!struct_def.fixed) {
      // Generate a special accessor for the table that has been declared as
      // the root type.
      NewRootTypeFromBuffer(struct_def, &code);
    }
    for (auto it = struct_def.fields.vec.begin();
         it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (field.deprecated) continue;

      GenStructAccessor(struct_def, field, &code, &dependencies);
    }
    if (struct_def.fixed) {
      // create a struct constructor function
      GenStructBuilder(struct_def, &code);
    } else {
      // Create a set of functions that allow table construction.
      GenTableBuilders(struct_def, &code);
    }
    EndStruct(&code);

    ns.addStruct(struct_def.defined_namespace, NormalizedName(struct_def),
                 dependencies, code);
  }

  // Generate enum declarations.
  void GenEnum(const EnumDef &enum_def) {
    StringSet dependencies;
    if (enum_def.generated) return;
    std::string type, of_int, to_int;
    const std::string &name = NormalizedName(enum_def);

    std::string code;

    GenComment(enum_def.doc_comment, &code);
    BeginEnum(name, &code);
    const auto &vals = enum_def.Vals();
    for (auto it = vals.begin(); it != vals.end(); ++it) {
      auto &ev = **it;
      GenComment(ev.doc_comment, &type);
      EnumMember(ev, type, of_int, to_int);
    }
    of_int += Indent + Indent + "| _ -> failwith \"Invalid value\"\n";
    code += Indent + " type t =\n";
    code += type;
    code += "\n";
    code += Indent + "let of_int = function\n";
    code += of_int;
    code += "\n";
    code += Indent + "let to_int = function\n";
    code += to_int;
    code += "\n";

    if (parser_.opts.generate_object_based_api) {
      if (enum_def.is_union) { generateUnion(enum_def, &dependencies, &code); }
    }

    EndEnum(&code);

    ns.addStruct(enum_def.defined_namespace, name, dependencies, code);
  }

  // Returns the method name for use with add/put calls.
  std::string GenMethod(const FieldDef &field) {
    return IsScalar(field.value.type.base_type)
               ? MakeCamel(GenTypeBasic(field.value.type))
               : (IsStruct(field.value.type) ? "Struct" : "UOffsetTRelative");
  }

  std::string GenTypePointer(const Type &type) {
    switch (type.base_type) {
      case BASE_TYPE_STRING: return "string";
      case BASE_TYPE_VECTOR: return GenTypeGet(type.VectorType());
      case BASE_TYPE_STRUCT: return type.struct_def->name;
      case BASE_TYPE_UNION:
        // fall through
      default: return "*flatbuffers.Table";
    }
  }

  std::string GenTypeGet(const Type &type) {
    return IsScalar(type.base_type) ? GenTypeBasic(type) : GenTypePointer(type);
  }

  std::string TypeName(const FieldDef &field) {
    return GenTypeGet(field.value.type);
  }

  // Create a struct with a builder and the struct's arguments.
  void GenStructBuilder(const StructDef &struct_def, std::string *code_ptr) {
    BeginBuilderArgs(struct_def, code_ptr);
    StructBuilderArgs(struct_def, "", code_ptr);
    EndBuilderArgs(code_ptr);
    StructBuilderBody(struct_def, "", code_ptr);
    EndBuilderBody(code_ptr);
  }

  bool generate() {
    if (!generateEnums()) return false;
    if (!generateStructs()) return false;
    std::string code;
    BeginFile(&code);
    ns.print(&code);
    return SaveFile(GeneratedFileName(path_, file_name_, parser_.opts).c_str(),
                    code, false);
  }

 private:
  void GenComment(const std::vector<std::string> &dc, std::string *code_ptr) {
    if (dc.begin() == dc.end()) {
      // Don't output empty comment blocks with 0 lines of comment content.
      return;
    }
    std::string &code = *code_ptr;
    if (dc.size() == 1) {
      code += "(* ";
      for (auto it = dc.begin(); it != dc.end(); ++it) { code += *it; }
      code += "*)\n";
    } else {
      code += "(*\n";
      for (auto it = dc.begin(); it != dc.end(); ++it) { code += *it + "\n"; }
      code += "\n*)\n";
    }
  }

  std::string getUnionName(const Type &type) {
    return NormalizedName(type) + ".Union";
  }

  void generateUnion(const EnumDef &enum_def, StringSet *dependencies,
                     std::string *code) {
    BeginModule("Union", code);
    *code += Indent + Indent + "type t = \n";
    const auto &vals = enum_def.Vals();
    for (auto it = vals.begin(); it != vals.end(); ++it) {
      auto &ev = **it;
      std::string name = NormalizedName(ev);
      if (ev.GetAsInt64() == 0) {
        *code += Indent + Indent + "  " + name + "\n";
      } else {
        std::string type_name = NormalizedName(ev.union_type);
        dependencies->insert(type_name);
        *code +=
            Indent + Indent + "| " + name + " of " + type_name + ".t" + "\n";
      }
    }

    EndModule(code);
  }

  bool generateEnums() {
    for (auto it = parser_.enums_.vec.begin(); it != parser_.enums_.vec.end();
         ++it) {
      auto &enum_def = **it;
      GenEnum(enum_def);
    }
    return true;
  }

  bool generateStructs() {
    for (auto it = parser_.structs_.vec.begin();
         it != parser_.structs_.vec.end(); ++it) {
      auto &struct_def = **it;
      GenStruct(struct_def);
    }
    return true;
  }

  void BeginFile(std::string *code_ptr) {
    std::string &code = *code_ptr;
    code = code + "(* " + FlatBuffersGeneratedWarning() + " *)\n\n";
    code = code + "open FlatBuffers\n\n";
    return;
  }

 private:
  std::unordered_set<std::string> keywords_;
  const SimpleFloatConstantGenerator float_const_gen_;
};

}  // namespace ocaml

bool GenerateOcaml(const Parser &parser, const std::string &path,
                   const std::string &file_name) {
  ocaml::OcamlGenerator generator(parser, path, file_name);
  return generator.generate();
}

}  // namespace flatbuffers
