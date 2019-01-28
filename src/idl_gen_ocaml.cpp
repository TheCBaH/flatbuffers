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

#include <string>

#include "flatbuffers/code_generators.h"
#include "flatbuffers/flatbuffers.h"
#include "flatbuffers/idl.h"
#include "flatbuffers/util.h"

#include <unordered_set>

namespace flatbuffers {
namespace ocaml {

// Hardcode spaces per indentation.
const std::string Indent = "    ";

const std::string kGeneratedFileNamePostfix = "_generated";

static std::string GeneratedFileName(const std::string &path,
                                     const std::string &file_name) {
  return path + file_name + kGeneratedFileNamePostfix + ".ml";
}


class Module {
private:
  void print(const std::vector<std::string> defs, std::string *code_ptr) {
    for(auto it = defs.begin(); it != defs.end(); it++) {
      *code_ptr += *it;
    }
  }
  void add(std::vector<std::string> &defs, const std::string &def) {
    defs.push_back(def);
  }
  std::vector<std::string> enums;
  std::vector<std::string> structs;
public:
  Module() {}
  void addEnum(const std::string &_enum) {
    add(enums, _enum);
  }
  void addStruct(const std::string &_struct) {
    add(structs, _struct);
  }
  void printEnums(std::string *code_ptr) {
    print(enums, code_ptr);
  }
  void printStructs(std::string *code_ptr) {
    print(structs, code_ptr);
  }
};

class Modules {
private:
  std::map<std::vector<std::string>, Module> map;
public:
  Modules () {}
  Module &get(const flatbuffers::Namespace *ns) {
    return map[ns->components];
  }
  void generate(std::string *code_ptr) {
    for(auto it = map.begin(); it != map.end(); it++) {
      auto ns = it->first;
      for (auto m = ns.begin(); m != ns.end(); ++m) {
	*code_ptr += "module " + *m + " = struct\n";
      }
      it->second.printEnums(code_ptr);
      it->second.printStructs(code_ptr);
      for (auto m = ns.begin(); m != ns.end(); ++m) {
	*code_ptr += "end\n\n";
      }
    }
  }
};

class OcamlGenerator : public BaseGenerator {
 private:
  Modules modules;
 public:
  OcamlGenerator(const Parser &parser, const std::string &path,
                  const std::string &file_name)
      : BaseGenerator(parser, path, file_name, "" /* not used */,
                      "" /* not used */),
        float_const_gen_("float('nan')", "float('inf')", "float('-inf')") {
    static const char * const keywords[] = {
      "False",
      "None",
      "True",
      "and",
      "as",
      "assert",
      "break",
      "class",
      "continue",
      "def",
      "del",
      "elif",
      "else",
      "except",
      "finally",
      "for",
      "from",
      "global",
      "if",
      "import",
      "in",
      "is",
      "lambda",
      "nonlocal",
      "not",
      "or",
      "pass",
      "raise",
      "return",
      "try",
      "while",
      "with",
      "yield"
    };
    keywords_.insert(std::begin(keywords), std::end(keywords));
  }

  // Most field accessors need to retrieve and test the field offset first,
  // this is the prefix code for that.
  std::string OffsetPrefix(const FieldDef &field) {
    return "\n" + Indent + Indent +
          "o = flatbuffers.number_types.UOffsetTFlags.py_type" +
          "(self._tab.Offset(" + NumToString(field.value.offset) + "))\n" +
          Indent + Indent + "if o != 0:\n";
  }

  void BeginModule(const std::string class_name, std::string *code_ptr) {
    *code_ptr += "module " + class_name + " = struct\n";
  }

  void BeginStruct(const StructDef &struct_def, std::string *code_ptr) {
    BeginModule(NormalizedName(struct_def), code_ptr);
    *code_ptr += Indent + "type t = {\n";
    *code_ptr += Indent + Indent + "b: ByteBuffer.t;\n";
    *code_ptr += Indent + Indent + "pos: int;\n";
    *code_ptr += Indent + "}\n";
    *code_ptr += "\n";
    *code_ptr += Indent + "let init b pos = {b;pos}\n\n";
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

  // A single enum member.
  void EnumMember(const EnumVal ev, std::string &type, std::string &of_int, std::string &to_int) {
    std::string name = NormalizedName(ev);
    std::string value = NumToString(ev.value);
    type += Indent + Indent + "| " + name + "\n";
    // code += NumToString(ev.value) + "\n";
    of_int += Indent + Indent + "| " + value + " -> " + name + "\n";
    to_int += Indent + Indent + "| " + name + " -> " + value + "\n";
    return;
  }

  void EndModule(std::string *code_ptr) {
    *code_ptr += "end\n\n";
  }

  void EndEnum(std::string *code_ptr) {
    EndModule(code_ptr);
  }

  void EndStruct(std::string *code_ptr) {
    EndModule(code_ptr);
  }

  // Initialize a new struct or table from existing data.
  void NewRootTypeFromBuffer(const StructDef &struct_def,
                             std::string *code_ptr) {
    std::string &code = *code_ptr;

    code += Indent + "@classmethod\n";
    code += Indent + "def GetRootAs";
    code += NormalizedName(struct_def);
    code += "(cls, buf, offset):";
    code += "\n";
    code += Indent + Indent;
    code += "n = flatbuffers.encode.Get";
    code += "(flatbuffers.packer.uoffset, buf, offset)\n";
    code += Indent + Indent + "x = " + NormalizedName(struct_def) + "()\n";
    code += Indent + Indent + "x.Init(buf, n + offset)\n";
    code += Indent + Indent + "return x\n";
    code += "\n";
  }

  // Initialize an existing object with other data, to avoid an allocation.
  void InitializeExisting(const StructDef &struct_def,
                          std::string *code_ptr) {
    std::string &code = *code_ptr;

    GenReceiver(struct_def, code_ptr);
    code += "Init(self, buf, pos):\n";
    code += Indent + Indent + "self._tab = flatbuffers.table.Table(buf, pos)\n";
    code += "\n";
  }

  // Get the length of a vector.
  void GetVectorLen(const StructDef &struct_def, const FieldDef &field,
                    std::string *code_ptr) {
    std::string &code = *code_ptr;

    GenReceiver(struct_def, code_ptr);
    code += MakeCamel(NormalizedName(field)) + "Length(self";
    code += "):" + OffsetPrefix(field);
    code += Indent + Indent + Indent + "return self._tab.VectorLen(o)\n";
    code += Indent + Indent + "return 0\n\n";
  }

  std::string GetScalarAccessorType(const StructDef &struct_def,
					   const FieldDef &field) {
    auto type = field.value.type;
    if(0 && &struct_def) {
      return std::string("not_supported");
    }
    return MakeCamel(GenTypeBasic(type));
  }

  std::string GetScalarReceiver(const StructDef &struct_def, const FieldDef &field)
  {
    std::string code;
    code += "ByteBuffer.";
    code += "read" + GetScalarAccessorType(struct_def, field) + " t.b offset";
    return code;
  }

  std::string GetStructReceiver(const StructDef &struct_def, const FieldDef &field)
  {
    std::string code;
    if(0 && &struct_def) {
      return std::string("not_supported");
    }
    code += MakeCamel(TypeName(field)) + ".init t.b offset";
    return code;
  }

  void GenOcamlReceiver(const FieldDef &field, std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += Indent + "let ";
    code += NormalizedName(field);
    code += " t =\n";
}

  // Get the value of a struct's scalar.
  void GetScalarFieldOfStruct(const StructDef &struct_def,
                              const FieldDef &field,
                              std::string *code_ptr) {
    GenOcamlReceiver(field, code_ptr);
    std::string &code = *code_ptr;
    code += Indent + Indent + "let offset = t.pos + " + NumToString(field.value.offset) + " in\n";
    code += Indent + Indent + GetScalarReceiver(struct_def, field) + "\n";
  }

  // Get the value of a table's scalar.
  void GetScalarFieldOfTable(const StructDef &struct_def,
                             const FieldDef &field,
                             std::string *code_ptr) {
    GenOcamlReceiver(field, code_ptr);
    std::string &code = *code_ptr;
    code += Indent + Indent + "let offset = ByteBuffer.__offset t.b t.pos " + NumToString(field.value.offset) + " in\n";
    std::string default_value;
    auto is_bool = IsBool(field.value.type.base_type);
    if (is_bool) {
      default_value = field.value.constant == "0" ? "false" : "true";
    } else {
      default_value = IsFloat(field.value.type.base_type)
                          ? float_const_gen_.GenFloatConstant(field)
                          : field.value.constant;
    }
    std::string field_value;
    field_value = GetScalarReceiver(struct_def, field) ;
    if(field.value.type.enum_def) {
      auto module_name = NormalizedName(*field.value.type.enum_def);
      field_value = module_name + ".of_int (" + field_value + ")";
      if (auto val = field.value.type.enum_def->ReverseLookup(StringToInt(field.value.constant.c_str()), false)) {
          default_value = module_name + "." + val->name;
      }
    }
    code += Indent + Indent + "if(offset!=0) then " + field_value + "\n";
    code += Indent + Indent + "else " + default_value + "\n";
  }

  // Get a struct by initializing an existing struct.
  // Specific to Struct.
  void GetStructFieldOfStruct(const StructDef &struct_def,
                              const FieldDef &field,
                              std::string *code_ptr) {
    GenOcamlReceiver(field, code_ptr);
    std::string &code = *code_ptr;
    code += Indent + Indent + "let offset = t.pos +" +  NumToString(field.value.offset) + " in\n";
    code += Indent + Indent + GetStructReceiver(struct_def, field) + "\n";

 #if 0
    GenReceiver(struct_def, code_ptr);
    code += NormalizedName(field);
    code += "(self, obj):\n";
    code += Indent + Indent + "obj.Init(self._tab.Bytes, self._tab.Pos + ";
    code += NumToString(field.value.offset) + ")";
    code += "\n" + Indent + Indent + "return obj\n\n";
#endif
  }

  // Get a struct by initializing an existing struct.
  // Specific to Table.
  void GetStructFieldOfTable(const StructDef &struct_def,
                             const FieldDef &field,
                             std::string *code_ptr) {
    GenOcamlReceiver(field, code_ptr);
    std::string &code = *code_ptr;
    code += Indent + Indent + "let offset = ";
    if (field.value.type.struct_def->fixed) {
      code += "ByteBuffer.__offset t.b t.pos " + NumToString(field.value.offset);
    } else {
      code += "t.pos + (ByteBuffer.__indirect t.b " + NumToString(field.value.offset) + ")";
    }
    code += " in\n";
    code += Indent + Indent + "if(offset!=0) then Some (" + GetStructReceiver(struct_def, field) + ")\n";
    code += Indent + Indent + "else None\n";
  }

  // Get the value of a string.
  void GetStringField(const StructDef &struct_def, const FieldDef &field,
                      std::string *code_ptr) {
    if(0 && &struct_def) {}
    GenOcamlReceiver(field, code_ptr);
    std::string &code = *code_ptr;
    code += Indent + Indent + "let offset = ByteBuffer.__offset t.b t.pos " + NumToString(field.value.offset) + " in\n";
    code += Indent + Indent + "if(offset!=0) then Some (ByteBuffer.__string t.b offset)\n";
    code += Indent + Indent + "else None\n";
  }

  // Get the value of a union from an object.
  void GetUnionField(const StructDef &struct_def, const FieldDef &field,
                     std::string *code_ptr) {

    if(0 && &struct_def) {}
    GenOcamlReceiver(field, code_ptr);
    std::string &code = *code_ptr;
    code += Indent + Indent + "let offset = ByteBuffer.__offset t.b t.pos " + NumToString(field.value.offset) + " in\n";
    code += Indent + Indent + "if(offset!=0) then ";
    code += MakeCamel(TypeName(field)) + ".of_int (ByteBuffer.readUint8 t.b offset)\n";
    std::string default_value;
    default_value = field.value.constant;
    code += Indent + Indent + "else " + default_value + "\n";

    #if 0
    GenReceiver(struct_def, code_ptr);
    code += MakeCamel(NormalizedName(field)) + "(self):";
    code += OffsetPrefix(field);

    // TODO(rw): this works and is not the good way to it:
    bool is_native_table = TypeName(field) == "*flatbuffers.Table";
    if (is_native_table) {
      code += Indent + Indent + Indent + "from flatbuffers.table import Table\n";
    } else {
      code += Indent + Indent + Indent;
      code += "from ." + TypeName(field) + " import " + TypeName(field) + "\n";
    }
    code += Indent + Indent + Indent + "obj = Table(bytearray(), 0)\n";
    code += Indent + Indent + Indent + GenGetter(field.value.type);
    code += "obj, o)\n" + Indent + Indent + Indent + "return obj\n";
    code += Indent + Indent + "return None\n\n";
    #endif
  }

  // Get the value of a vector's struct member.
  void GetMemberOfVectorOfStruct(const StructDef &struct_def,
                                 const FieldDef &field,
                                 std::string *code_ptr) {
    std::string &code = *code_ptr;
    auto vectortype = field.value.type.VectorType();

    GenReceiver(struct_def, code_ptr);
    code += MakeCamel(NormalizedName(field));
    code += "(self, j):" + OffsetPrefix(field);
    code += Indent + Indent + Indent + "x = self._tab.Vector(o)\n";
    code += Indent + Indent + Indent;
    code += "x += flatbuffers.number_types.UOffsetTFlags.py_type(j) * ";
    code += NumToString(InlineSize(vectortype)) + "\n";
    if (!(vectortype.struct_def->fixed)) {
      code += Indent + Indent + Indent + "x = self._tab.Indirect(x)\n";
    }
    code += Indent + Indent + Indent;
    code += "from ." + TypeName(field) + " import " + TypeName(field) + "\n";
    code += Indent + Indent + Indent + "obj = " + TypeName(field) + "()\n";
    code += Indent + Indent + Indent + "obj.Init(self._tab.Bytes, x)\n";
    code += Indent + Indent + Indent + "return obj\n";
    code += Indent + Indent + "return None\n\n";
  }

  // Get the value of a vector's non-struct member. Uses a named return
  // argument to conveniently set the zero value for the result.
  void GetMemberOfVectorOfNonStruct(const StructDef &struct_def,
                                    const FieldDef &field,
                                    std::string *code_ptr) {
    std::string &code = *code_ptr;
    auto vectortype = field.value.type.VectorType();

    GenReceiver(struct_def, code_ptr);
    code += MakeCamel(NormalizedName(field));
    code += "(self, j):";
    code += OffsetPrefix(field);
    code += Indent + Indent + Indent + "a = self._tab.Vector(o)\n";
    code += Indent + Indent + Indent;
    code += "return " + GenGetter(field.value.type);
    code += "a + flatbuffers.number_types.UOffsetTFlags.py_type(j * ";
    code += NumToString(InlineSize(vectortype)) + "))\n";
    if (vectortype.base_type == BASE_TYPE_STRING) {
      code += Indent + Indent + "return \"\"\n";
    } else {
      code += Indent + Indent + "return 0\n";
    }
    code += "\n";
  }

  // Returns a non-struct vector as a numpy array. Much faster
  // than iterating over the vector element by element.
  void GetVectorOfNonStructAsNumpy(const StructDef &struct_def,
                                   const FieldDef &field,
                                   std::string *code_ptr) {
    std::string &code = *code_ptr;
    auto vectortype = field.value.type.VectorType();

    // Currently, we only support accessing as numpy array if
    // the vector type is a scalar.
    if (!(IsScalar(vectortype.base_type))) { return; }

    GenReceiver(struct_def, code_ptr);
    code += MakeCamel(NormalizedName(field)) + "AsNumpy(self):";
    code += OffsetPrefix(field);

    code += Indent + Indent + Indent;
    code += "return ";
    code += "self._tab.GetVectorAsNumpy(flatbuffers.number_types.";
    code += MakeCamel(GenTypeGet(field.value.type));
    code += "Flags, o)\n";

    if (vectortype.base_type == BASE_TYPE_STRING) {
      code += Indent + Indent + "return \"\"\n";
    } else {
      code += Indent + Indent + "return 0\n";
    }
    code += "\n";
  }

  // Begin the creator function signature.
  void BeginBuilderArgs(const StructDef &struct_def,
                        std::string *code_ptr) {
    std::string &code = *code_ptr;

    code += "\n";
    code += "def Create" + NormalizedName(struct_def);
    code += "(builder";
  }

  // Recursively generate arguments for a constructor, to deal with nested
  // structs.
  void StructBuilderArgs(const StructDef &struct_def,
                         const char *nameprefix, std::string *code_ptr) {
    for (auto it = struct_def.fields.vec.begin();
        it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (IsStruct(field.value.type)) {
        // Generate arguments for a struct inside a struct. To ensure names
        // don't clash, and to make it obvious these arguments are constructing
        // a nested struct, prefix the name with the field name.
        StructBuilderArgs(*field.value.type.struct_def,
                          (nameprefix + (NormalizedName(field) + "_")).c_str(), code_ptr);
      } else {
        std::string &code = *code_ptr;
        code += std::string(", ") + nameprefix;
        code += MakeCamel(NormalizedName(field), false);
      }
    }
  }

  // End the creator function signature.
  void EndBuilderArgs(std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += "):\n";
  }

  // Recursively generate struct construction statements and instert manual
  // padding.
  void StructBuilderBody(const StructDef &struct_def,
                         const char *nameprefix, std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += "    builder.Prep(" + NumToString(struct_def.minalign) + ", ";
    code += NumToString(struct_def.bytesize) + ")\n";
    for (auto it = struct_def.fields.vec.rbegin();
        it != struct_def.fields.vec.rend(); ++it) {
      auto &field = **it;
      if (field.padding)
        code += "    builder.Pad(" + NumToString(field.padding) + ")\n";
      if (IsStruct(field.value.type)) {
        StructBuilderBody(*field.value.type.struct_def,
                          (nameprefix + (NormalizedName(field) + "_")).c_str(), code_ptr);
      } else {
        code += "    builder.Prepend" + GenMethod(field) + "(";
        code += nameprefix + MakeCamel(NormalizedName(field), false) + ")\n";
      }
    }
  }

  void EndBuilderBody(std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += "    return builder.Offset()\n";
  }

  // Get the value of a table's starting offset.
  void GetStartOfTable(const StructDef &struct_def,
                       std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += "def " + NormalizedName(struct_def) + "Start";
    code += "(builder): ";
    code += "builder.StartObject(";
    code += NumToString(struct_def.fields.vec.size());
    code += ")\n";
  }

  // Set the value of a table's field.
  void BuildFieldOfTable(const StructDef &struct_def,
                         const FieldDef &field, const size_t offset,
                         std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += "def " + NormalizedName(struct_def) + "Add" + MakeCamel(NormalizedName(field));
    code += "(builder, ";
    code += MakeCamel(NormalizedName(field), false);
    code += "): ";
    code += "builder.Prepend";
    code += GenMethod(field) + "Slot(";
    code += NumToString(offset) + ", ";
    if (!IsScalar(field.value.type.base_type) && (!struct_def.fixed)) {
      code += "flatbuffers.number_types.UOffsetTFlags.py_type";
      code += "(";
      code += MakeCamel(NormalizedName(field), false) + ")";
    } else {
      code += MakeCamel(NormalizedName(field), false);
    }
    code += ", ";
    code += IsFloat(field.value.type.base_type)
                ? float_const_gen_.GenFloatConstant(field)
                : field.value.constant;
    code += ")\n";
  }

  // Set the value of one of the members of a table's vector.
  void BuildVectorOfTable(const StructDef &struct_def,
                          const FieldDef &field, std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += "def " + NormalizedName(struct_def) + "Start";
    code += MakeCamel(NormalizedName(field));
    code += "Vector(builder, numElems): return builder.StartVector(";
    auto vector_type = field.value.type.VectorType();
    auto alignment = InlineAlignment(vector_type);
    auto elem_size = InlineSize(vector_type);
    code += NumToString(elem_size);
    code += ", numElems, " + NumToString(alignment);
    code += ")\n";
  }

  // Get the offset of the end of a table.
  void GetEndOffsetOnTable(const StructDef &struct_def,
                           std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += "def " + NormalizedName(struct_def) + "End";
    code += "(builder): ";
    code += "return builder.EndObject()\n";
  }

  // Generate the receiver for function signatures.
  void GenReceiver(const StructDef &struct_def, std::string *code_ptr) {
    std::string &code = *code_ptr;
    code += Indent + "# " + NormalizedName(struct_def) + "\n";
    code += Indent + "def ";
  }

  // Generate a struct field, conditioned on its child type(s).
  void GenStructAccessor(const StructDef &struct_def,
                         const FieldDef &field, std::string *code_ptr) {
    GenComment(field.doc_comment, code_ptr);
    if (IsScalar(field.value.type.base_type)) {
      if (struct_def.fixed) {
        GetScalarFieldOfStruct(struct_def, field, code_ptr);
      } else {
        GetScalarFieldOfTable(struct_def, field, code_ptr);
      }
    }
    else {
      switch (field.value.type.base_type) {
        case BASE_TYPE_STRUCT:
          if (struct_def.fixed) {
            GetStructFieldOfStruct(struct_def, field, code_ptr);
          } else {
            GetStructFieldOfTable(struct_def, field, code_ptr);
          }
          break;
        case BASE_TYPE_STRING: GetStringField(struct_def, field, code_ptr); break;
	  #if 0
        case BASE_TYPE_VECTOR: {
          auto vectortype = field.value.type.VectorType();
          if (vectortype.base_type == BASE_TYPE_STRUCT) {
            GetMemberOfVectorOfStruct(struct_def, field, code_ptr);
          } else {
            GetMemberOfVectorOfNonStruct(struct_def, field, code_ptr);
            GetVectorOfNonStructAsNumpy(struct_def, field, code_ptr);
          }
          break;
        }
        case BASE_TYPE_UNION: GetUnionField(struct_def, field, code_ptr); break;
	  #endif
        default:
	  #if 0
	  FLATBUFFERS_ASSERT(0);
	  #endif
	  break;
      }
    }
    #if 0
    if (field.value.type.base_type == BASE_TYPE_VECTOR) {
      GetVectorLen(struct_def, field, code_ptr);
    }
    #endif
    *code_ptr += "\n";
  }

  // Generate table constructors, conditioned on its members' types.
  void GenTableBuilders(const StructDef &struct_def,
                        std::string *code_ptr) {
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

    GenComment(struct_def.doc_comment, &code);
    BeginStruct(struct_def, &code);
#if 0
    if (!struct_def.fixed) {
      // Generate a special accessor for the table that has been declared as
      // the root type.
      NewRootTypeFromBuffer(struct_def, code_ptr);
    }
    // Generate the Init method that sets the field in a pre-existing
    // accessor object. This is to allow object reuse.
    InitializeExisting(struct_def, code_ptr);
#endif
    for (auto it = struct_def.fields.vec.begin();
        it != struct_def.fields.vec.end(); ++it) {
      auto &field = **it;
      if (field.deprecated) continue;

      GenStructAccessor(struct_def, field, &code);
    }
#if 0

    if (struct_def.fixed) {
      // create a struct constructor function
      GenStructBuilder(struct_def, code_ptr);
    } else {
      // Create a set of functions that allow table construction.
      GenTableBuilders(struct_def, code_ptr);
    }
#endif
    EndStruct(&code);

    auto &m = modules.get(struct_def.defined_namespace);
    m.addStruct(code);
  }

  // Generate enum declarations.
  void GenEnum(const EnumDef &enum_def) {
    if (enum_def.generated) return;
    std::string type, of_int, to_int;

    std::string code;

    GenComment(enum_def.doc_comment, &code);
    BeginEnum(NormalizedName(enum_def), &code);
    for (auto it = enum_def.vals.vec.begin(); it != enum_def.vals.vec.end();
        ++it) {
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

    EndEnum(&code);

    auto &m = modules.get(enum_def.defined_namespace);
    m.addEnum(code);
  }

  // Returns the function name that is able to read a value of the given type.
  std::string GenGetter(const Type &type) {
    switch (type.base_type) {
      case BASE_TYPE_STRING: return "self._tab.String(";
      case BASE_TYPE_UNION: return "self._tab.Union(";
      case BASE_TYPE_VECTOR: return GenGetter(type.VectorType());
      default:
        return "self._tab.Get(flatbuffers.number_types." +
              MakeCamel(GenTypeGet(type)) + "Flags, ";
    }
  }

  // Returns the method name for use with add/put calls.
  std::string GenMethod(const FieldDef &field) {
    return IsScalar(field.value.type.base_type)
              ? MakeCamel(GenTypeBasic(field.value.type))
              : (IsStruct(field.value.type) ? "Struct" : "UOffsetTRelative");
  }

  std::string GenTypeBasic(const Type &type) {
    static const char *ctypename[] = {
    // clang-format off
      #define FLATBUFFERS_TD(ENUM, IDLTYPE, \
        CTYPE, JTYPE, GTYPE, NTYPE, PTYPE, RTYPE) \
        #PTYPE,
        FLATBUFFERS_GEN_TYPES(FLATBUFFERS_TD)
      #undef FLATBUFFERS_TD
      // clang-format on
    };
    return ctypename[type.base_type];
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
  void GenStructBuilder(const StructDef &struct_def,
                              std::string *code_ptr) {
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
    modules.generate(&code);
    return SaveFile(GeneratedFileName(path_, file_name_).c_str(), code,
                    false);
  }

 private:

  void GenComment(const std::vector<std::string> &dc, std::string *code_ptr)
  {
    if (dc.begin() == dc.end()) {
      // Don't output empty comment blocks with 0 lines of comment content.
      return;
    }
    std::string &code = *code_ptr;
    if(dc.size() == 1) {
      code += "(* ";
      for (auto it = dc.begin(); it != dc.end(); ++it) {
	code += *it;
      }
      code += "*)\n";
    } else {
      code += "(*\n";
      for (auto it = dc.begin(); it != dc.end(); ++it) {
	code += *it + "\n";
      }
      code += "\n*)\n";
    }
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

  // Begin by declaring namespace and imports.
  void BeginFile(const std::string name_space_name, const bool needs_imports,
                 std::string *code_ptr) {
    std::string &code = *code_ptr;
    code = code + "(* " + FlatBuffersGeneratedWarning() + " *)\n\n";
    code += "(* namespace: " + name_space_name + " *)\n\n";
    if (needs_imports) { code += "import flatbuffers\n\n"; }
  }

  // Save out the generated code for a Ocaml Table type.
  bool SaveType(const Definition &def, const std::string &classcode,
                bool needs_imports) {
    if (!classcode.length()) return true;

    std::string namespace_dir = path_;
    auto &namespaces = def.defined_namespace->components;
    for (auto it = namespaces.begin(); it != namespaces.end(); ++it) {
      if (it != namespaces.begin()) namespace_dir += kPathSeparator;
      namespace_dir += *it;
      std::string init_py_filename = namespace_dir + "/__init__.py";
      SaveFile(init_py_filename.c_str(), "", false);
    }

    std::string code = "";
    BeginFile(LastNamespacePart(*def.defined_namespace), needs_imports, &code);
    code += classcode;
    std::string filename =
      NamespaceDir(*def.defined_namespace) + NormalizedName(def) + ".ml";
    return SaveFile(filename.c_str(), code, false);
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
