/*
 * Copyright 2021 Google Inc. All rights reserved.
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

#include "bfbs_gen_ocaml.h"

#include <algorithm>
#include <cstdint>
#include <iostream>
#include <map>
#include <memory>
#include <queue>
#include <string>
#include <unordered_set>
#include <vector>

// Ensure no includes to flatc internals. bfbs_gen.h and generator.h are OK.
#include "bfbs_gen.h"
#include "bfbs_namer.h"

// The intermediate representation schema.
#include "flatbuffers/flexbuffers.h"
#include "flatbuffers/idl.h"
#include "flatbuffers/reflection.h"
#include "flatbuffers/reflection_generated.h"
#include "flatbuffers/util.h"

namespace flatbuffers {
namespace {

// To reduce typing
namespace r = ::reflection;

std::set<std::string> OCamlKeywords() {
  return { // OCaml reserved words
           "and",     "as",         "assert",    "asr",         "begin",
           "class",   "constraint", "do",        "done",        "downto",
           "else",    "end",        "exception", "external",    "false",
           "for",     "fun",        "function",  "functor",     "if",
           "in",      "include",    "inherit",   "initializer", "land",
           "lazy",    "let",        "lor",       "lsl",         "lsr",
           "lxor",    "match",      "method",    "mod",         "module",
           "mutable", "new",        "nonrec",    "object",      "of",
           "open",    "or",         "private",   "rec",         "sig",
           "struct",  "then",       "to",        "true",        "try",
           "type",    "val",        "virtual",   "when",        "while",
           "with",
           // Generator-synthesized identifiers (snake_case — fields/functions)
           "obj", "unpack", "pack", "to_string", "extension", "identifier",
           "root", "finish_buf", "has_identifier", "lookup_by_key",
           // Generator-synthesized identifiers (UpperCamel — modules)
           "Rt", "Builder", "Vector", "Vector64" };
}

Namer::Config OCamlDefaultConfig() {
  return { /*types=*/Case::kSnake2,
           /*constants=*/Case::kSnake2,
           /*methods=*/Case::kSnake2,
           /*functions=*/Case::kSnake2,
           /*fields=*/Case::kSnake2,
           /*variables=*/Case::kSnake2,
           /*variants=*/Case::kSnake2,
           /*enum_variant_seperator=*/"",
           /*escape_keywords=*/Namer::Config::Escape::AfterConvertingCase,
           /*namespaces=*/Case::kUpperCamel,
           /*namespace_seperator=*/".",
           /*object_prefix=*/"",
           /*object_suffix=*/"",
           /*keyword_prefix=*/"",
           /*keyword_suffix=*/"_",
           /*keywords_casing=*/Namer::Config::KeywordsCasing::CaseSensitive,
           /*filenames=*/Case::kSnake2,
           /*directories=*/Case::kKeep,
           /*output_path=*/"",
           /*filename_suffix=*/"",
           /*filename_extension=*/".ml" };
}

class OCamlBfbsGenerator : public BaseBfbsGenerator {
 public:
  explicit OCamlBfbsGenerator(const std::string &flatc_version)
      : BaseBfbsGenerator(),
        root_node_(),
        ident_counter_(0),
        enum_reader_idents_(),
        flatc_version_(flatc_version),
        namer_(OCamlDefaultConfig(), OCamlKeywords()) {}

  Status GenerateFromSchema(const r::Schema *schema,
                            const CodeGenOptions &options) FLATBUFFERS_OVERRIDE {
    options_ = options;
    // build single module tree for all definitions
    ForAllObjects(schema->objects(), [&](const r::Object *object) {
      auto node = AddNode(object->name()->str());
      node->object = object;
    });
    ForAllEnums(schema->enums(), [&](const r::Enum *enum_def) {
      auto node = AddNode(enum_def->name()->str());
      node->enum_def = enum_def;
    });

    std::string intf, impl;
    EmitCodeHeader(intf, impl);

    // generate out-of-line definitions to avoid references between modules
    GenerateStructSetFns(impl);
    GenerateUnionReadFns(impl);

    // generate user-facing modules
    EmitCode(intf, impl);

    WriteFiles(intf, impl);
    return OK;
  }

  using BaseBfbsGenerator::GenerateCode;

  Status GenerateCode(const Parser &parser, const std::string &path,
                      const std::string &filename) FLATBUFFERS_OVERRIDE {
    (void)parser;
    (void)path;
    (void)filename;
    return NOT_IMPLEMENTED;
  }

  Status GenerateMakeRule(const Parser &parser, const std::string &path,
                          const std::string &filename,
                          std::string &output) override {
    (void)parser;
    (void)path;
    (void)filename;
    (void)output;
    return Status::NOT_IMPLEMENTED;
  }

  Status GenerateGrpcCode(const Parser &parser, const std::string &path,
                          const std::string &filename) override {
    (void)parser;
    (void)path;
    (void)filename;
    return Status::NOT_IMPLEMENTED;
  }

  Status GenerateRootFile(const Parser &parser,
                          const std::string &path) override {
    (void)parser;
    (void)path;
    return Status::NOT_IMPLEMENTED;
  }

  bool IsSchemaOnly() const override { return true; }

  bool SupportsBfbsGeneration() const override { return true; }

  bool SupportsRootFileGeneration() const override { return false; }

  IDLOptions::Language Language() const override { return IDLOptions::kOCaml; }

  std::string LanguageName() const override { return "OCaml"; }

  uint64_t SupportedAdvancedFeatures() const FLATBUFFERS_OVERRIDE {
    return r::AdvancedArrayFeatures | r::AdvancedUnionFeatures |
           r::OptionalScalars | r::DefaultVectorsAndStrings;
  }

 private:
  const std::string RuntimeNS = "Rt";

  std::string UnionReadIdent(const r::Enum *enum_def) {
    const auto enum_name = enum_def->name()->str();
    std::string read_fn = enum_reader_idents_[enum_name];
    if (read_fn.empty()) {
      read_fn = "read_table_" + namer_.Function(namer_.Denamespace(enum_name)) +
                "__" + NumToString(ident_counter_++);
      enum_reader_idents_[enum_name] = read_fn;
    }
    return read_fn;
  }

  std::string StructSetIdent(const r::Object *object) {
    const auto object_name = object->name()->str();
    std::string repr = enum_reader_idents_[object_name];
    if (repr.empty()) {
      repr = "set_" + namer_.Function(namer_.Denamespace(object_name)) + "__" +
             NumToString(ident_counter_++);
      enum_reader_idents_[object_name] = repr;
    }
    return repr;
  }

  // Get FQ names of objects/enums that an object's type definition depends on
  std::set<std::string> GetDependencies(const r::Object *object) {
    std::set<std::string> deps;
    const std::string obj_name = object->name()->str();
    ForAllFields(object, false, [&](const r::Field *field) {
      if (field->deprecated()) return;
      const r::BaseType bt = field->type()->base_type();
      if (IsScalar(bt) && field->type()->index() >= 0) {
        deps.insert(GetEnum(field->type())->name()->str());
      }
      if (bt == r::Obj) {
        auto name = GetObject(field->type())->name()->str();
        if (name != obj_name) deps.insert(name);
      }
      if ((bt == r::Vector || bt == r::Vector64)) {
        if (field->type()->element() == r::Obj) {
          auto name = GetObject(field->type(), true)->name()->str();
          if (name != obj_name) deps.insert(name);
        }
        if (IsScalar(field->type()->element()) && field->type()->index() >= 0) {
          deps.insert(GetEnum(field->type())->name()->str());
        }
      }
      if (bt == r::Union) {
        auto enum_def = GetEnum(field->type());
        deps.insert(enum_def->name()->str());
        ForAllEnumValues(enum_def, [&](const r::EnumVal *e) {
          if (e->union_type()->base_type() == r::Obj) {
            auto name = GetObject(e->union_type())->name()->str();
            if (name != obj_name) deps.insert(name);
          }
        });
      }
      if (bt == r::Array) {
        if (field->type()->element() == r::Obj) {
          auto name = GetObject(field->type(), true)->name()->str();
          if (name != obj_name) deps.insert(name);
        }
        if (IsScalar(field->type()->element()) && field->type()->index() >= 0) {
          deps.insert(GetEnum(field->type())->name()->str());
        }
      }
    });
    return deps;
  }

  void GenerateUnionReadFns(std::string &impl) {
    const auto indent = Indent(1);

    std::vector<const r::Enum *> unions;
    ForAllEnums(schema_->enums(), [&](const r::Enum *enum_def) {
      if (enum_def->is_union()) unions.push_back(enum_def);
    });

    if (unions.empty()) return;

    impl += "module Union = struct";
    for (auto enum_def : unions) {
      const std::string ns = GenerateImplNs(enum_def->underlying_type());
      const std::string reader_name = UnionReadIdent(enum_def);

      const std::string args = GenerateUnionArgs(enum_def->underlying_type());
      impl += "\n" + indent + "let " + reader_name + args + " b i t o =\n";
      impl += indent + "  match " + ns + ".to_default t with\n";
      ForAllEnumValues(enum_def, [&](const reflection::EnumVal *e) {
        auto arg = namer_.Variable(e->name()->str());
        impl += indent + "  | " + Int64ToString(e->value()) +
                " when Option.is_some " + arg + " -> ";
        impl += "Option.get " + arg;
        if (e->union_type()->base_type() == r::None) {
          impl += "\n";
        } else {
          impl +=
              " (" + GenerateImplNs(e->union_type()) + ".read_table b o i)\n";
        }
      });
      impl += indent + "  | _ -> default t\n";
    }
    impl += "end\n\n";
  }

  void GenerateStructSetFns(std::string &impl) {
    const auto indent = Indent(1);

    std::vector<const r::Object *> structs;
    ForAllObjects(schema_->objects(), [&](const r::Object *object) {
      if (object->is_struct()) structs.push_back(object);
    });

    if (structs.empty()) return;

    // Topologically sort structs so dependencies come first, allowing
    // simple `let` instead of `let rec ... and ...`.
    std::map<std::string, int> name_to_idx;
    for (size_t i = 0; i < structs.size(); i++)
      name_to_idx[structs[i]->name()->str()] = static_cast<int>(i);

    std::vector<int> order;
    std::vector<int> state(structs.size(), 0);  // 0=unvisited, 1=visiting, 2=done
    std::function<void(int)> visit = [&](int i) {
      if (state[i] == 2) return;
      state[i] = 1;
      ForAllFields(structs[i], false, [&](const r::Field *field) {
        const r::Object *dep = nullptr;
        if (field->type()->base_type() == r::Obj) {
          dep = GetObject(field->type());
        } else if (field->type()->base_type() == r::Array &&
                   field->type()->element() == r::Obj) {
          dep = GetObject(field->type(), true);
        }
        if (dep && dep->is_struct()) {
          auto it = name_to_idx.find(dep->name()->str());
          if (it != name_to_idx.end() && state[it->second] != 1)
            visit(it->second);
        }
      });
      state[i] = 2;
      order.push_back(i);
    };
    for (size_t i = 0; i < structs.size(); i++) visit(static_cast<int>(i));

    impl += "module Struct = struct\n";
    for (auto idx : order) {
      auto object = structs[idx];
      std::string set_args;
      std::string set_body;
      bool first_field = true;
      ForAllFields(object, /*reverse=*/false, [&](const r::Field *field) {
        const auto arg_name = namer_.Variable(field->name()->str());
        set_args += (first_field ? "" : ", ") + arg_name + "_";

        if (field->type()->base_type() == r::Array) {
          auto elem_type = field->type()->element();
          uint32_t elem_size = field->type()->element_size();
          std::string set_fn;
          if (IsScalar(elem_type)) {
            set_fn = RuntimeNS + ".Builder.set_scalar T" +
                     r::EnumNameBaseType(elem_type);
          } else if (elem_type == r::Obj) {
            auto obj = GetObject(field->type(), true);
            set_fn = StructSetIdent(obj);
            elem_size = obj->bytesize();
          }
          set_body += indent + "  Array.iteri (fun j_ v_ -> " + set_fn +
                      " b (i + " + NumToString(field->offset()) +
                      " + j_ * " + NumToString(elem_size) +
                      ") v_) " + arg_name + "_;\n";
        } else {
          set_body += indent + "  " + StructSetFn(field->type()) + " b (i + " +
                      NumToString(field->offset()) + ") " + arg_name + "_;\n";
        }
        if (field->padding() != 0) {
          auto pad_offset = field->offset();
          if (field->type()->base_type() == r::Array) {
            uint32_t actual_elem_size = field->type()->element_size();
            if (field->type()->element() == r::Obj) {
              actual_elem_size = GetObject(field->type(), true)->bytesize();
            }
            pad_offset += field->type()->fixed_length() * actual_elem_size;
          } else {
            pad_offset += field->type()->base_size();
          }
          set_body +=
              indent + "  Rt.Builder.set_padding b (i + " +
              NumToString(pad_offset) + ") " +
              NumToString(field->padding()) + ";\n";
        }

        first_field = false;
      });

      impl += "\n" + indent + "let " + StructSetIdent(object) + " b i (" +
              set_args + ") =\n" + set_body + indent + "  ()\n";
    }
    impl += "end\n\n";
  }

  void GenerateEnum(const r::Enum *enum_def, int level, std::string &intf,
                    std::string &impl) {
    const auto indent = Indent(level);
    const std::string ns = GenerateImplNs(enum_def->underlying_type());
    const std::string type = ns + ".t";

    intf += indent + "type t = private " + type + "\n\n";
    impl += indent + "type t = " + type + "\n\n";

    // enum constants
    ForAllEnumValues(enum_def, [&](const reflection::EnumVal *e) {
      const auto comment = GenerateDocumentation(e->documentation(), level);
      if (!comment.empty()) intf += "\n" + comment;
      intf += indent + "val " + namer_.Variant(e->name()->str()) + " : t\n";
      impl += indent + "let " + namer_.Variant(e->name()->str()) + " = " + ns +
              ".of_default " + Int64ToString(e->value()) + "\n";
    });

    // to_string for enums and union tags
    intf += indent + "val to_string : t -> string\n";
    impl += "\n" + indent + "let to_string e =\n";
    impl += indent + "  match " + ns + ".to_default e with\n";
    ForAllEnumValues(enum_def, [&](const reflection::EnumVal *e) {
      impl += indent + "  | " + Int64ToString(e->value()) + " -> \"" +
              namer_.Variant(e->name()->str()) + "\"\n";
    });
    impl += indent + "  | x -> \"<" + enum_def->name()->str() +
            ": \" ^ (Int64.to_string x) ^ \">\"\n";

    if (!enum_def->is_union()) {
      // vector module
      intf += "\n" + indent +
              "module Vector : Rt.VectorS with type 'b elt := t and type "
              "builder_elt := t\n";
      impl += "\n" + indent + "module Vector = " + ns + ".Vector\n";
      intf += indent +
              "module Vector64 : Rt.VectorS with type 'b elt := t and type "
              "builder_elt := t\n";
      impl += indent + "module Vector64 = " + ns + ".Vector64\n";
    } else {
      // Object API: inline polymorphic variant type
      const std::string enum_name = enum_def->name()->str();
      std::string union_def = indent + "type obj = [\n";
      ForAllEnumValues(enum_def, [&](const r::EnumVal *e) {
        const auto variant = ObjVariantName(e->name()->str());
        if (e->union_type()->base_type() == r::None) {
          union_def += indent + "  | `" + variant + "\n";
        } else if (e->union_type()->base_type() == r::Obj) {
          auto obj = GetObject(e->union_type());
          auto rel = NamespaceRelComponents(obj->name()->str(), enum_name);
          union_def += indent + "  | `" + variant + " of " +
                       NsRef(rel, "obj") + "\n";
        } else if (e->union_type()->base_type() == r::String) {
          union_def += indent + "  | `" + variant + " of string\n";
        }
      });
      union_def += indent + "]\n";
      intf += "\n" + union_def;
      impl += "\n" + union_def;
    }
  }

  void GenerateObject(const r::Object *object, int level, std::string &intf,
                      std::string &impl) {
    const std::string indent = Indent(level);
    const std::string obj_name = object->name()->str();
    std::string obj_ns;
    const std::string obj_shortname = namer_.Denamespace(obj_name, obj_ns);

    if (object->is_struct()) {
      intf +=
          indent + "type t = " + StructReprArgTypes(object, obj_ns) + "\n\n";
      impl += indent + "type t = " + StructReprArgTypes(object, obj_ns, false) +
              "\n\n";
    } else {
      intf += indent + "type t\n\n";
      impl += indent + "type t\n\n";
    }

    // vector module
    if (object->is_struct()) {
      std::string struct_functor_arg =
              " (struct type builder_elt = t let size = " +
              NumToString(object->bytesize()) + " let minalign = " +
              NumToString(object->minalign()) + " let set = Struct." +
              StructSetIdent(object) + " end)";
      intf += indent +
              "module Vector : Rt.VectorS with type 'b elt := ('b, t) " +
              RuntimeNS + ".fb and type builder_elt := t\n\n";
      impl += indent + "module Vector = " + RuntimeNS +
              ".Struct.Vector" + struct_functor_arg + "\n\n";
      intf += indent +
              "module Vector64 : Rt.VectorS with type 'b elt := ('b, t) " +
              RuntimeNS + ".fb and type builder_elt := t\n\n";
      impl += indent + "module Vector64 = " + RuntimeNS +
              ".Struct.Vector64" + struct_functor_arg + "\n\n";
    } else {
      intf += indent +
              "module Vector : Rt.VectorS with type 'b elt := ('b, t) " +
              RuntimeNS + ".fb and type builder_elt := " + "t " + RuntimeNS +
              ".wip" + "\n\n";
      impl += indent + "module Vector = " + RuntimeNS + ".Ref.Vector\n\n";
      intf += indent +
              "module Vector64 : Rt.VectorS with type 'b elt := ('b, t) " +
              RuntimeNS + ".fb and type builder_elt := " + "t " + RuntimeNS +
              ".wip" + "\n\n";
      impl += indent + "module Vector64 = " + RuntimeNS + ".Ref64.Vector\n\n";
    }

    // root table
    if (schema_->root_table() == object) {
      intf += indent + "val extension : string option\n";
      auto ident = schema_->file_ext()->str();
      impl += indent + "let extension = " +
              (ident.empty() ? "None" : "Some \"" + ident + "\"") + "\n";

      intf += indent + "val identifier : string option\n";
      auto ext = schema_->file_ident()->str();
      impl += indent + "let identifier = " +
              (ext.empty() ? "None" : "Some \"" + ext + "\"") + "\n";

      if (!ident.empty()) {
        intf += indent +
                "val has_identifier : ?size_prefixed:bool -> ?off:int -> "
                "'b Flatbuffers.Primitives.t -> 'b -> bool\n";
        impl +=
            indent +
            "let has_identifier ?(size_prefixed = false) ?(off = 0) p b = " +
            RuntimeNS +
            ".get_identifier p b ~size_prefixed ~off = Option.get identifier\n";
      }

      intf += indent +
              "val root : ?size_prefixed:bool -> ?off:int -> 'b "
              "Flatbuffers.Primitives.t "
              "-> 'b -> t " +
              RuntimeNS + ".root\n";
      impl += indent +
              "let[@inline] root ?(size_prefixed = false) ?(off = 0) p b = " +
              RuntimeNS + ".get_root p b ~size_prefixed ~off\n";

      if (schema_->root_table() == object) {
        intf +=
            indent +
            "val finish_buf : ?size_prefixed:bool -> 'a "
            "Flatbuffers.Primitives.t -> Rt.Builder.t -> t Rt.wip -> 'a\n\n";
        impl += indent +
                "let finish_buf ?(size_prefixed = false) = " + RuntimeNS +
                ".Builder.finish ?identifier ~size_prefixed\n\n";
      }
    }

    // create all the field accessors.
    ForAllFields(object, /*reverse=*/false, [&](const r::Field *field) {
      if (field->deprecated()) { return; }

      const auto comment = GenerateDocumentation(field->documentation(), level);
      if (!comment.empty()) intf += "\n" + comment;

      const std::string field_name = namer_.Field(*field);
      const r::BaseType field_type = field->type()->base_type();

      // fixed-length array fields (only in structs)
      if (field_type == r::Array && object->is_struct()) {
        auto fixed_len = field->type()->fixed_length();
        auto elem_type = field->type()->element();
        // element_size from reflection is the base type size, not the
        // struct bytesize. Use the object's bytesize for struct elements.
        uint32_t elem_size = field->type()->element_size();
        if (elem_type == r::Obj) {
          elem_size = GetObject(field->type(), true)->bytesize();
        }

        // length constant
        intf += indent + "val " + namer_.Function(field_name) +
                "_length : int\n";
        impl += indent + "let " + namer_.Function(field_name) +
                "_length = " + NumToString(fixed_len) + "\n";

        // element reader type for interface
        std::string elem_reader_type;
        if (IsImmediateType(field->type(), true)) {
          elem_reader_type =
              GenerateIntfNs(field->type(), obj_name, true) + ".t";
        } else {
          elem_reader_type = "('b, " +
              GenerateIntfNs(field->type(), obj_name, true) +
              ".t) " + RuntimeNS + ".fb";
        }

        // indexed accessor
        intf += indent + "val " + namer_.Function(field_name) +
                " : 'b " + RuntimeNS + ".buf -> ('b, t) " + RuntimeNS +
                ".fb -> int -> " + elem_reader_type + "\n";
        impl += indent + "let[@inline] " + namer_.Function(field_name) +
                " b s i = " +
                GenerateImplNs(field->type(), true) +
                ".read_offset b s (" + NumToString(field->offset()) +
                " + i * " + NumToString(elem_size) + ")\n";
        return;
      }

      // generate accessor interface
      if (field_type == r::Union) {
        auto args = GenerateUnionArgTypes(field->type(), obj_name);
        intf += indent + "val " + namer_.Function(field_name) + " :" + args +
                " 'b Rt.buf -> ('b, t) " + RuntimeNS + ".fb -> 'a\n";
      } else {
        // For DefaultVectorsAndStrings: non-optional, non-required reference
        // fields are read as optional since the default isn't in the buffer.
        bool reader_opt = field->optional() && !object->is_struct();
        if (!object->is_struct() && !field->optional() && !field->required() &&
            !IsImmediateType(field->type())) {
          reader_opt = true;
        }
        intf += indent + "val " + namer_.Function(field_name) +
                " : 'b Rt.buf -> ('b, t) " + RuntimeNS + ".fb -> " +
                GenerateReaderType(
                    field->type(), obj_name, reader_opt) +
                "\n";
      }

      // generate accessor implementation
      if (object->is_struct()) {
        impl += indent + "let[@inline] " + namer_.Function(field_name) +
                " b s = " + GenerateImplNs(field->type()) +
                ".read_offset b s " + NumToString(field->offset()) + "\n";
      } else {
        if (field_type == r::Union) {
          auto ns = GenerateIntfNs(field->type(), obj_name);
          auto args = GenerateUnionArgs(field->type());
          // When the tag vtable slot is absent, read_table_default returns 0
          // (None). The union reader's match handles this: if ~none is provided
          // it fires; otherwise the catch-all ~default receives the None tag.
          impl += indent + "let[@inline] " + namer_.Function(field_name) +
                  args + " b o = Union." +
                  UnionReadIdent(GetEnum(field->type())) + " b " +
                  NumToString(field->offset()) + " (" + field_name +
                  UnionTypeFieldSuffix() + " b o)" + args + " o\n";
        } else {
          std::string getter_fn, default_arg;
          bool is_offset64 = field->offset64();
          if (field->optional()) {
            getter_fn = ".read_table_opt";
          } else if (field->required()) {
            getter_fn = ".read_table";
          } else if (!IsImmediateType(field->type())) {
            // DefaultVectorsAndStrings: string/vector/object fields with
            // defaults are read as optional at the zero-copy level since
            // the default value is not stored in the buffer.
            getter_fn = ".read_table_opt";
          } else {
            getter_fn = ".(read_table_default";
            default_arg = " ~default:(" + GenerateDefault(field) + "))";
          }
          impl += indent + "let[@inline] " + namer_.Function(field_name) +
                  " b o = " + GenerateImplNs(field->type(), false, is_offset64) +
                  getter_fn + " b o " + NumToString(field->offset()) +
                  default_arg + "\n";
        }
      }

      // generate nested flatbuffer accessor if applicable
      if (!object->is_struct()) {
        auto nested_type = GetNestedFlatbuffer(field);
        if (!nested_type.empty()) {
          auto nested_obj = FindObjectByName(nested_type, obj_name);
          if (nested_obj) {
            auto rel = NamespaceRelComponents(
                nested_obj->name()->str(), obj_name);
            // If empty (self-referential), use just "t"; otherwise qualify
            std::string type_ref;
            if (rel.empty()) {
              type_ref = "t";
            } else {
              type_ref = namer_.Namespace(rel) + ".t";
            }

            // interface: returns a root of the nested type
            intf += indent + "val " + namer_.Function(field_name) +
                    "_as_" + namer_.Function(nested_type) +
                    " : 'b Rt.buf -> ('b, t) " + RuntimeNS + ".fb -> " +
                    type_ref + " " + RuntimeNS + ".root option\n";

            // implementation: read vector, extract as nested root
            impl += indent + "let[@inline] " + namer_.Function(field_name) +
                    "_as_" + namer_.Function(nested_type) +
                    " b o = " + RuntimeNS + ".Option.fold" +
                    " ~none:None" +
                    " ~some:(fun v -> Some (" + RuntimeNS +
                    ".get_nested_root b v))" +
                    " (" + namer_.Function(field_name) + " b o)\n";
          }
        }
      }
    });

    // generate lookup_by_key if object has a key field
    {
      const r::Field *key_field = nullptr;
      ForAllFields(object, /*reverse=*/false, [&](const r::Field *field) {
        if (field->key()) key_field = field;
      });

      if (key_field) {
        const std::string key_name = namer_.Field(*key_field);
        const r::BaseType key_type = key_field->type()->base_type();
        const bool is_string_key = (key_type == r::String);

        // Determine key type for interface
        std::string key_type_str;
        if (is_string_key) {
          key_type_str = "string";
        } else {
          key_type_str = GenerateType(key_field->type(), obj_name);
        }

        // interface
        intf += indent + "val lookup_by_key : 'b " + RuntimeNS +
                ".buf -> ('b, Vector.t) " + RuntimeNS + ".fb -> " +
                key_type_str + " -> ('b, t) " + RuntimeNS + ".fbopt\n";

        // implementation
        std::string cmp_expr;
        if (is_string_key) {
          cmp_expr = "String.compare (" + RuntimeNS +
                     ".String.to_string buf (" + namer_.Function(key_name) +
                     " buf elt)) key";
        } else {
          cmp_expr = "compare (" + namer_.Function(key_name) +
                     " buf elt) key";
        }

        if (object->is_struct()) {
          impl += indent + "let[@inline] lookup_by_key buf vec_off key = " +
                  RuntimeNS + ".lookup_by_key_struct ~size:" +
                  NumToString(object->bytesize()) + " ~minalign:" +
                  NumToString(object->minalign()) + " " +
                  "buf vec_off " +
                  "(fun elt -> " + cmp_expr + ")\n";
        } else {
          impl += indent + "let[@inline] lookup_by_key buf vec_off key = " +
                  RuntimeNS + ".lookup_by_key_ref buf vec_off " +
                  "(fun elt -> " + cmp_expr + ")\n";
        }
      }
    }

    if (!object->is_struct()) {
      // generate builder
      const std::string indent2 = Indent(level + 1);

      impl += "\n" + indent + "module Builder = struct\n";
      impl += indent2 + "type t = Rt.Builder.t\n\n";

      intf += "\n" + indent + "module Builder : sig\n";
      intf += indent2 + "type t\n\n";

      impl += indent2 + "let start b = Rt.Builder.start_table b ~n_fields:" +
              NumToString(object->fields()->size()) + "\n";
      impl += indent2 + "let finish b = Rt.Builder.end_table b\n";

      intf += indent2 + "val start : Rt.Builder.t -> t\n";
      intf += indent2 + "val finish : t -> " + namer_.Namespace(obj_shortname) + ".t Rt.wip\n";

      ForAllFields(object, /*reverse=*/false, [&](const r::Field *field) {
        // Skip writing deprecated fields altogether.
        if (field->deprecated()) { return; }

        const std::string field_name = namer_.Field(*field);
        const r::BaseType field_type = field->type()->base_type();

        if (field_type == r::UType) {
          return;  // no builder function for tags
        } else if (field_type == r::Union) {
          ForAllEnumValues(GetEnum(field->type()), [&](const r::EnumVal *e) {
            if (e->union_type()->base_type() == r::None) return;
            const auto variant_name = namer_.Variant(e->name()->str());
            impl += indent2 + "let add_" + namer_.Function(field_name) + "_" +
                    variant_name + " = Rt.Ref.push_union " +
                    NumToString(field->id() - 1) + " " +
                    NumToString(field->id()) + " " +
                    GenerateIntfNs(field->type(), obj_ns) + "." + variant_name +
                    "\n";
            intf += indent2 + "val add_" + namer_.Function(field_name) + "_" +
                    variant_name + " : " +
                    GenerateBuilderType(e->union_type(), obj_ns) +
                    " -> t -> t\n";
          });
        } else {
          // struct fields take an extra arg
          std::string push_fn, repr_arg, default_arg;
          bool is_offset64 = field->offset64();
          if (IsStruct(field->type())) {
            auto object = GetObject(field->type());
            repr_arg = " Struct." + StructSetIdent(object) + " " +
                       NumToString(object->bytesize()) + " " +
                       NumToString(object->minalign());
          }
          if (!field->optional() && !field->required() &&
              IsImmediateType(field->type())) {
            push_fn = ".(push_slot_default";
            default_arg = " ~default:(" + GenerateDefault(field) + "))";
          } else {
            push_fn = ".push_slot";
          }
          impl += indent2 + "let add_" + namer_.Function(field_name) + " = " +
                  GenerateImplNs(field->type(), false, is_offset64) + push_fn +
                  repr_arg + " " + NumToString(field->id()) + default_arg + "\n";
          intf += indent2 + "val add_" + namer_.Function(field_name) + " : " +
                  GenerateBuilderType(field->type(), obj_ns) + " -> t -> t\n";
        }
      });
      impl += indent + "end\n";
      intf += indent + "end\n";
    }

    // ===== Object API =====
    {
      // Count non-deprecated, non-UType fields
      int n_obj_fields = 0;
      ForAllFields(object, false, [&](const r::Field *field) {
        if (!field->deprecated() && field->type()->base_type() != r::UType)
          n_obj_fields++;
      });

      // Define type obj inline
      if (n_obj_fields == 0) {
        impl += "\n" + indent + "type obj = unit\n\n";
        intf += "\n" + indent + "type obj = unit\n\n";
      } else {
        std::string obj_def = indent + "type obj = {\n";
        ForAllFields(object, false, [&](const r::Field *field) {
          if (field->deprecated()) return;
          if (field->type()->base_type() == r::UType) return;
          std::string fname = namer_.Field(*field);
          std::string ftype = ObjFieldType(field, obj_name, object->is_struct());
          obj_def += indent + "  " + fname + " : " + ftype + ";\n";
        });
        obj_def += indent + "}\n\n";
        impl += "\n" + obj_def;
        intf += "\n" + obj_def;
      }

      if (n_obj_fields == 0) {
        if (object->is_struct()) {
          intf += indent + "val unpack : 'b Rt.buf -> ('b, t) Rt.fb -> obj\n";
          impl += indent + "let unpack _b _s = ()\n";
          intf += indent + "val pack : obj -> t\n";
          impl += indent + "let pack () = ()\n";
        } else {
          intf += indent +
                  "val unpack : 'b Rt.buf -> ('b, t) Rt.fb -> obj\n";
          impl += indent + "let unpack _b _o = ()\n";
          intf += indent +
                  "val pack : Rt.Builder.t -> obj -> t Rt.wip\n";
          impl += indent +
                  "let pack b () =\n";
          impl += indent + "  let t = Builder.start b in\n";
          impl += indent + "  Builder.finish t\n";
        }

      } else {
        bool can_gen_fns = CanGenerateObjFns(object);
        bool self_ref = HasSelfReference(object);

        if (can_gen_fns && object->is_struct()) {
          // Struct unpack
          intf +=
              indent + "val unpack : 'b Rt.buf -> ('b, t) Rt.fb -> obj\n";
          impl += indent + "let unpack b__ s__ : obj = {\n";
          ForAllFields(object, false, [&](const r::Field *field) {
            if (field->deprecated()) return;
            const std::string fname = namer_.Field(*field);
            const r::BaseType bt = field->type()->base_type();
            std::string expr;

            if (bt == r::Array) {
              auto elem = field->type()->element();
              if (IsScalar(elem)) {
                expr = "Array.init " + fname + "_length (fun i -> " + fname +
                       " b__ s__ i)";
              } else if (elem == r::Obj) {
                auto obj = GetObject(field->type(), true);
                auto rel =
                    NamespaceRelComponents(obj->name()->str(), obj_name);
                expr = "Array.init " + fname + "_length (fun i -> " +
                       NsRef(rel, "unpack") + " b__ (" + fname +
                       " b__ s__ i))";
              }
            } else if (IsScalar(bt)) {
              expr = fname + " b__ s__";
            } else if (bt == r::Obj) {
              auto rel = NamespaceRelComponents(
                  GetObject(field->type())->name()->str(), obj_name);
              expr = NsRef(rel, "unpack") + " b__ (" + fname + " b__ s__)";
            }

            impl +=
                indent + "  " + fname + " = " + expr + ";\n";
          });
          impl += indent + "}\n\n";

          // Struct pack
          intf += indent + "val pack : obj -> t\n";
          int field_count = 0;
          ForAllFields(object, false, [&](const r::Field *field) {
            if (!field->deprecated()) field_count++;
          });

          impl += indent + "let pack (obj : obj) = ";
          if (field_count > 1) impl += "(";
          bool first = true;
          ForAllFields(object, false, [&](const r::Field *field) {
            if (field->deprecated()) return;
            const std::string fname = namer_.Field(*field);
            const r::BaseType bt = field->type()->base_type();
            std::string expr;

            if (bt == r::Array) {
              auto elem = field->type()->element();
              if (IsScalar(elem)) {
                expr = "obj." + fname;
              } else if (elem == r::Obj) {
                auto obj = GetObject(field->type(), true);
                auto rel =
                    NamespaceRelComponents(obj->name()->str(), obj_name);
                expr = "Array.map " + NsRef(rel, "pack") + " obj." +
                       fname;
              }
            } else if (IsScalar(bt)) {
              expr = "obj." + fname;
            } else if (bt == r::Obj) {
              auto rel = NamespaceRelComponents(
                  GetObject(field->type())->name()->str(), obj_name);
              expr = NsRef(rel, "pack") + " obj." + fname;
            }

            if (!first) impl += ", ";
            impl += expr;
            first = false;
          });
          if (field_count > 1) impl += ")";
          impl += "\n";
  
        } else if (can_gen_fns && !object->is_struct()) {
          // Table unpack
          intf +=
              indent + "val unpack : 'b Rt.buf -> ('b, t) Rt.fb -> obj\n";
          std::string rec_kw = self_ref ? "rec " : "";
          impl += indent + "let " + rec_kw + "unpack b__ o__ : obj = {\n";
          ForAllFields(object, false, [&](const r::Field *field) {
            if (field->deprecated()) return;
            if (field->type()->base_type() == r::UType) return;
            const std::string fname = namer_.Field(*field);
            std::string expr = TableUnpackExpr(field, obj_name);
            impl +=
                indent + "  " + fname + " = " + expr + ";\n";
          });
          impl += indent + "}\n\n";

          // Table pack
          intf += indent +
                  "val pack : Rt.Builder.t -> obj -> t Rt.wip\n";
          std::string pack_rec = self_ref ? "rec " : "";
          // If unpack used 'rec', pack needs 'and' instead of 'let rec'
          if (self_ref) {
            impl += indent + "and pack b__ (obj : obj) =\n";
          } else {
            impl += indent + "let pack b__ (obj : obj) =\n";
          }

          // Prepare phase: create offsets
          ForAllFields(object, false, [&](const r::Field *field) {
            if (field->deprecated()) return;
            if (field->type()->base_type() == r::UType) return;
            TablePackPrepare(field, obj_name, level, impl);
          });

          // Build phase
          impl += indent + "  let t = Builder.start b__ in\n";
          ForAllFields(object, false, [&](const r::Field *field) {
            if (field->deprecated()) return;
            if (field->type()->base_type() == r::UType) return;
            TablePackAdd(field, obj_name, level, impl);
          });
          impl += indent + "  Builder.finish t\n";
  
        }
        // else: skip unpack/pack (forward reference issue)
      }
    }
  }

  // ===== Object API helpers =====

  // Format a namespace-relative reference with a member suffix.
  // When rel is empty (self-reference), returns just the member name.
  std::string NsRef(const std::vector<std::string> &rel,
                     const std::string &member) {
    if (rel.empty()) return member;
    return namer_.Namespace(rel) + "." + member;
  }

  std::string ObjVariantName(const std::string &name) {
    std::string result = namer_.Namespace(name);
    if (result == "None") result = "None_";
    return result;
  }

  bool HasSelfReference(const r::Object *object) {
    bool self_ref = false;
    const std::string obj_name = object->name()->str();
    ForAllFields(object, false, [&](const r::Field *field) {
      if (field->deprecated()) return;
      const r::BaseType bt = field->type()->base_type();
      if (bt == r::Obj) {
        if (GetObject(field->type())->name()->str() == obj_name) self_ref = true;
      }
      if ((bt == r::Vector || bt == r::Vector64) &&
          field->type()->element() == r::Obj) {
        if (GetObject(field->type(), true)->name()->str() == obj_name)
          self_ref = true;
      }
      if (bt == r::Union) {
        ForAllEnumValues(GetEnum(field->type()), [&](const r::EnumVal *e) {
          if (e->union_type()->base_type() == r::Obj &&
              GetObject(e->union_type())->name()->str() == obj_name)
            self_ref = true;
        });
      }
    });
    return self_ref;
  }

  bool CanGenerateObjFns(const r::Object *) {
    return true;  // All dependencies visible via module rec
  }

  // OCaml type for a field in an obj record
  std::string ObjFieldType(const r::Field *field, const std::string &in_ns,
                            bool is_struct) {
    const r::BaseType bt = field->type()->base_type();

    // Fixed-length array
    if (bt == r::Array) {
      auto elem = field->type()->element();
      std::string et;
      if (IsScalar(elem)) {
        if (field->type()->index() >= 0)
          et = GenerateIntfNs(field->type(), in_ns, true) + ".t";
        else
          et = RuntimeNS + "." + r::EnumNameBaseType(elem) + ".t";
      } else if (elem == r::Obj) {
        auto obj = GetObject(field->type(), true);
        auto rel = NamespaceRelComponents(obj->name()->str(), in_ns);
        et = NsRef(rel, "obj");
      }
      return et + " array";
    }

    // Scalar (plain or enum)
    if (IsScalar(bt)) {
      std::string t;
      if (field->type()->index() >= 0)
        t = GenerateIntfNs(field->type(), in_ns) + ".t";
      else
        t = RuntimeNS + "." + r::EnumNameBaseType(bt) + ".t";
      if (!is_struct && field->optional()) t += " option";
      return t;
    }

    // String
    if (bt == r::String) {
      std::string t = "string";
      if (!is_struct && field->optional()) t += " option";
      return t;
    }

    // Object ref
    if (bt == r::Obj) {
      auto object = GetObject(field->type());
      auto rel = NamespaceRelComponents(object->name()->str(), in_ns);
      std::string t = NsRef(rel, "obj");
      if (!is_struct && !field->required()) t += " option";
      return t;
    }

    // Vector
    if (bt == r::Vector || bt == r::Vector64) {
      auto elem = field->type()->element();
      std::string et;
      if (elem == r::String) {
        et = "string";
      } else if (IsScalar(elem)) {
        if (field->type()->index() >= 0)
          et = GenerateIntfNs(field->type(), in_ns, true) + ".t";
        else
          et = RuntimeNS + "." + r::EnumNameBaseType(elem) + ".t";
      } else if (elem == r::Obj) {
        auto obj = GetObject(field->type(), true);
        auto rel = NamespaceRelComponents(obj->name()->str(), in_ns);
        et = NsRef(rel, "obj");
      }
      return et + " array";
    }

    // Union
    if (bt == r::Union) {
      auto enum_def = GetEnum(field->type());
      auto rel = NamespaceRelComponents(enum_def->name()->str(), in_ns);
      return NsRef(rel, "obj");
    }

    return "{{ ERROR ObjFieldType }}";
  }

  // Generate table field unpack expression
  std::string TableUnpackExpr(const r::Field *field,
                               const std::string &in_ns) {
    const r::BaseType bt = field->type()->base_type();
    const std::string fname = namer_.Field(*field);

    // Scalar
    if (IsScalar(bt)) {
      return fname + " b__ o__";
    }

    // String
    if (bt == r::String) {
      if (field->optional()) {
        return RuntimeNS + ".Option.fold ~none:None ~some:(fun s -> Some (" +
               RuntimeNS + ".String.to_string b__ s)) (" + fname + " b__ o__)";
      } else if (field->required()) {
        return RuntimeNS + ".String.to_string b__ (" + fname + " b__ o__)";
      } else {
        // DefaultVectorsAndStrings: fold with empty string default
        return RuntimeNS + ".Option.fold ~none:\"\" ~some:(fun s -> " +
               RuntimeNS + ".String.to_string b__ s) (" + fname + " b__ o__)";
      }
    }

    // Object ref (table or struct)
    if (bt == r::Obj) {
      auto object = GetObject(field->type());
      auto rel = NamespaceRelComponents(object->name()->str(), in_ns);
      std::string unpack_fn = NsRef(rel, "unpack");
      if (field->required()) {
        return unpack_fn + " b__ (" + fname + " b__ o__)";
      } else {
        return RuntimeNS + ".Option.fold ~none:None ~some:(fun x -> Some (" +
               unpack_fn + " b__ x)) (" + fname + " b__ o__)";
      }
    }

    // Vector
    if (bt == r::Vector || bt == r::Vector64) {
      auto elem = field->type()->element();
      std::string vec_ns = GenerateIntfNs(field->type(), in_ns);

      std::string body;
      if (elem == r::String) {
        body = "Array.map (fun s -> " + RuntimeNS +
               ".String.to_string b__ s) (" + vec_ns + ".to_array b__ v)";
      } else if (IsScalar(elem)) {
        body = vec_ns + ".to_array b__ v";
      } else if (elem == r::Obj) {
        auto obj = GetObject(field->type(), true);
        auto rel = NamespaceRelComponents(obj->name()->str(), in_ns);
        std::string uf = NsRef(rel, "unpack");
        body = "Array.map (fun x -> " + uf + " b__ x) (" + vec_ns +
               ".to_array b__ v)";
      }

      if (field->required()) {
        return "(let v = " + fname + " b__ o__ in " + body + ")";
      } else {
        return RuntimeNS + ".Option.fold ~none:[||] ~some:(fun v -> " +
               body + ") (" + fname + " b__ o__)";
      }
    }

    // Union
    if (bt == r::Union) {
      auto enum_def = GetEnum(field->type());

      std::string expr = fname;
      ForAllEnumValues(enum_def, [&](const r::EnumVal *e) {
        const auto variant = ObjVariantName(e->name()->str());
        if (e->union_type()->base_type() == r::None) {
          expr += " ~none:`" + variant;
        } else if (e->union_type()->base_type() == r::Obj) {
          auto obj = GetObject(e->union_type());
          auto rel = NamespaceRelComponents(obj->name()->str(), in_ns);
          std::string uf = NsRef(rel, "unpack");
          expr += " ~" + namer_.Variable(e->name()->str()) +
                  ":(fun x -> `" + variant + " (" + uf +
                  " b__ x))";
        } else if (e->union_type()->base_type() == r::String) {
          expr += " ~" + namer_.Variable(e->name()->str()) +
                  ":(fun x -> `" + variant + " (" +
                  RuntimeNS + ".String.to_string b__ x))";
        }
      });
      expr += " ~default:(fun _ -> `" +
              ObjVariantName("NONE") + ") b__ o__";
      return expr;
    }

    return "{{ ERROR TableUnpackExpr }}";
  }

  // Generate table pack prepare (offset creation before Builder.start)
  void TablePackPrepare(const r::Field *field, const std::string &in_ns,
                         int level, std::string &impl) {
    const r::BaseType bt = field->type()->base_type();
    const std::string fname = namer_.Field(*field);
    const std::string ind = Indent(level + 1);

    // Scalars/enums: no offset needed
    if (IsScalar(bt)) return;

    // Struct in table: inline, no offset needed
    if (bt == r::Obj && GetObject(field->type())->is_struct()) return;

    // String
    if (bt == r::String) {
      if (field->optional()) {
        impl += ind + "let " + fname + "' = Option.map (fun s -> " +
                RuntimeNS + ".String.create b__ s) obj." + fname + " in\n";
      } else {
        impl += ind + "let " + fname + "' = " + RuntimeNS +
                ".String.create b__ obj." + fname + " in\n";
      }
      return;
    }

    // Table ref
    if (bt == r::Obj) {
      auto object = GetObject(field->type());
      auto rel = NamespaceRelComponents(object->name()->str(), in_ns);
      std::string pf = NsRef(rel, "pack");
      if (field->required()) {
        impl += ind + "let " + fname + "' = " + pf + " b__ obj." + fname +
                " in\n";
      } else {
        impl += ind + "let " + fname + "' = Option.map (fun x -> " + pf +
                " b__ x) obj." + fname + " in\n";
      }
      return;
    }

    // Vector
    if (bt == r::Vector || bt == r::Vector64) {
      auto elem = field->type()->element();
      bool is_v64 = (bt == r::Vector64);
      std::string vs = is_v64 ? "64" : "";

      if (elem == r::String) {
        impl += ind + "let " + fname + "' = " + RuntimeNS +
                ".String.Vector.create b__ (Array.map (fun s -> " + RuntimeNS +
                ".String.create b__ s) obj." + fname + ") in\n";
      } else if (IsScalar(elem)) {
        std::string sns;
        if (field->type()->index() >= 0)
          sns = GenerateIntfNs(field->type(), in_ns, true);
        else
          sns = RuntimeNS + "." + r::EnumNameBaseType(elem);
        impl += ind + "let " + fname + "' = " + sns + ".Vector" + vs +
                ".create b__ obj." + fname + " in\n";
      } else if (elem == r::Obj) {
        auto obj = GetObject(field->type(), true);
        auto rel = NamespaceRelComponents(obj->name()->str(), in_ns);
        std::string pack_fn = NsRef(rel, "pack");
        std::string vec_mod = NsRef(rel, "Vector" + vs);
        if (obj->is_struct()) {
          impl += ind + "let " + fname + "' = " + vec_mod +
                  ".create b__ (Array.map " + pack_fn + " obj." + fname +
                  ") in\n";
        } else {
          impl += ind + "let " + fname + "' = " + vec_mod +
                  ".create b__ (Array.map (fun x -> " + pack_fn +
                  " b__ x) obj." + fname + ") in\n";
        }
      }
      return;
    }

    // Union
    if (bt == r::Union) {
      auto enum_def = GetEnum(field->type());

      impl += ind + "let " + fname + "' = match obj." + fname + " with\n";
      ForAllEnumValues(enum_def, [&](const r::EnumVal *e) {
        const auto variant = ObjVariantName(e->name()->str());
        if (e->union_type()->base_type() == r::None) {
          impl += ind + "  | `" + variant + " -> None\n";
        } else if (e->union_type()->base_type() == r::Obj) {
          auto obj = GetObject(e->union_type());
          auto rel = NamespaceRelComponents(obj->name()->str(), in_ns);
          std::string pf = NsRef(rel, "pack");
          impl += ind + "  | `" + variant +
                  " x -> Some (`" + variant + ", " + pf + " b__ x)\n";
        } else if (e->union_type()->base_type() == r::String) {
          impl += ind + "  | `" + variant +
                  " x -> Some (`" + variant + ", " + RuntimeNS +
                  ".String.create b__ x)\n";
        }
      });
      impl += ind + "in\n";
      return;
    }
  }

  // Generate table pack add (after Builder.start)
  void TablePackAdd(const r::Field *field, const std::string &in_ns,
                     int level, std::string &impl) {
    const r::BaseType bt = field->type()->base_type();
    const std::string fname = namer_.Field(*field);
    const std::string ind = Indent(level + 1);

    // Scalar (non-optional)
    if (IsScalar(bt) && !field->optional()) {
      impl += ind + "let t = Builder.add_" + fname + " obj." + fname +
              " t in\n";
      return;
    }

    // Optional scalar
    if (IsScalar(bt) && field->optional()) {
      impl += ind + "let t = match obj." + fname +
              " with None -> t | Some v -> Builder.add_" + fname +
              " v t in\n";
      return;
    }

    // Struct in table (inline)
    if (bt == r::Obj && GetObject(field->type())->is_struct()) {
      auto object = GetObject(field->type());
      auto rel = NamespaceRelComponents(object->name()->str(), in_ns);
      std::string pf = NsRef(rel, "pack");
      if (field->required()) {
        impl += ind + "let t = Builder.add_" + fname + " (" + pf +
                " obj." + fname + ") t in\n";
      } else {
        impl += ind + "let t = match obj." + fname +
                " with None -> t | Some s -> Builder.add_" + fname +
                " (" + pf + " s) t in\n";
      }
      return;
    }

    // String (prepared offset)
    if (bt == r::String) {
      if (field->optional()) {
        impl += ind + "let t = match " + fname +
                "' with None -> t | Some off -> Builder.add_" + fname +
                " off t in\n";
      } else {
        impl += ind + "let t = Builder.add_" + fname + " " + fname +
                "' t in\n";
      }
      return;
    }

    // Table ref (prepared offset)
    if (bt == r::Obj) {
      if (field->required()) {
        impl += ind + "let t = Builder.add_" + fname + " " + fname +
                "' t in\n";
      } else {
        impl += ind + "let t = match " + fname +
                "' with None -> t | Some off -> Builder.add_" + fname +
                " off t in\n";
      }
      return;
    }

    // Vector (prepared offset)
    if (bt == r::Vector || bt == r::Vector64) {
      impl += ind + "let t = Builder.add_" + fname + " " + fname +
              "' t in\n";
      return;
    }

    // Union (prepared match)
    if (bt == r::Union) {
      auto enum_def = GetEnum(field->type());
      impl += ind + "let t = match " + fname + "' with\n";
      impl += ind + "  | None -> t\n";
      ForAllEnumValues(enum_def, [&](const r::EnumVal *e) {
        if (e->union_type()->base_type() == r::None) return;
        const auto variant = ObjVariantName(e->name()->str());
        const auto variant_fn = namer_.Variable(e->name()->str());
        impl += ind + "  | Some (`" + variant +
                ", off) -> Builder.add_" + fname + "_" + variant_fn +
                " off t\n";
      });
      impl += ind + "in\n";
      return;
    }
  }

  // ===== End Object API helpers =====

  std::string Int64ToString(int64_t x) const {
    if (x < 0)
      return "(" + NumToString(x) + "L)";
    else
      return NumToString(x) + "L";
  }

  std::string FloatToString(float x) const {
    if (x != x)
      return "nan";
    else if (x == std::numeric_limits<double>::infinity())
      return "infinity";
    else if (x == -std::numeric_limits<double>::infinity())
      return "neg_infinity";

    if (x < 0.0)
      return "(" + NumToString(x) + ")";
    else
      return NumToString(x);
  }

  std::string GenerateDefault(const r::Field *field) const {
    const r::BaseType base_type = field->type()->base_type();
    const std::string f = "of_default ";
    if (IsBool(base_type))
      return f + (field->default_integer() ? "true" : "false");
    else if (IsInteger(base_type))
      return f + Int64ToString(field->default_integer());
    else if (IsFloatingPoint(base_type))
      return f + FloatToString(field->default_real());
    else
      // should be unreachable
      return "{{ ERROR GenerateDefault }}";
  }

  // Get the nested_flatbuffer attribute value from a field, or empty string
  std::string GetNestedFlatbuffer(const r::Field *field) {
    auto attrs = field->attributes();
    if (!attrs) return "";
    auto kv = attrs->LookupByKey("nested_flatbuffer");
    if (!kv || !kv->value()) return "";
    return kv->value()->str();
  }

  // Find an object by its short name (e.g. "Monster") matching the field's
  // declaring namespace first, then falling back to any match
  const r::Object *FindObjectByName(const std::string &name,
                                     const std::string &in_ns) {
    const r::Object *fallback = nullptr;
    std::string ns_prefix;
    // Extract namespace from in_ns (everything before last dot/component)
    auto labels = NamespaceComponents(in_ns);
    labels.pop_back();  // remove the type name itself

    ForAllObjects(schema_->objects(), [&](const r::Object *obj) {
      auto obj_name = obj->name()->str();
      auto obj_labels = NamespaceComponents(obj_name);
      if (obj_labels.back() == name) {
        // Check if namespaces match
        auto obj_ns = obj_labels;
        obj_ns.pop_back();
        if (obj_ns == labels) {
          fallback = obj;  // exact namespace match
        } else if (!fallback) {
          fallback = obj;  // first match as fallback
        }
      }
    });
    return fallback;
  }

  // these are NOT represented by an offset (RT.fb, RT.fbopt) in the runtime:
  // scalars, enums (incl union tags)
  bool IsImmediateType(const r::Type *type, bool element_type = false) {
    const r::BaseType base_type =
        element_type ? type->element() : type->base_type();
    return IsScalar(base_type);
  }

  // in_ns should be an object/enum name as-is
  std::string GenerateIntfNs(const r::Type *type, const std::string &in_ns,
                             bool element_type = false) {
    const r::BaseType base_type =
        element_type ? type->element() : type->base_type();
    if ((IsScalar(base_type) && type->index() < 0) || base_type == r::String) {
      return RuntimeNS + "." + r::EnumNameBaseType(base_type);
    } else if (base_type == r::Vector || base_type == r::Vector64) {
      auto ns = GenerateIntfNs(type, in_ns, /*elt_type=*/true);
      if (!ns.empty()) ns += ".";
      return ns + (base_type == r::Vector64 ? "Vector64" : "Vector");
    } else if (base_type == r::Obj) {
      auto object = GetObject(type, element_type);
      return namer_.Namespace(
          NamespaceRelComponents(object->name()->str(), in_ns));
    } else if (base_type == r::Union || IsInteger(base_type)) {
      auto enum_def = GetEnum(type, element_type);
      return namer_.Namespace(
          NamespaceRelComponents(enum_def->name()->str(), in_ns));
    } else {
      return "{{ ERROR GenerateIntfNs }}\n";
    }
  }

  // namespace of reader implementations: Rt.Ref, Rt.Ref64, Rt.Struct, or Rt.Scalar
  std::string GenerateImplNs(const r::Type *type, bool element_type = false,
                              bool offset64 = false) {
    const r::BaseType base_type =
        element_type ? type->element() : type->base_type();
    if (IsScalar(base_type)) {
      return RuntimeNS + "." + r::EnumNameBaseType(base_type);
    } else if (base_type == r::Vector || base_type == r::Vector64 ||
               base_type == r::String) {
      return RuntimeNS + ((offset64 || base_type == r::Vector64) ? ".Ref64" : ".Ref");
    } else if (base_type == r::Obj) {
      auto object = GetObject(type, element_type);
      if (object->is_struct())
        return RuntimeNS + ".Struct";
      else
        return RuntimeNS + (offset64 ? ".Ref64" : ".Ref");
    } else if (base_type == r::Union) {
      const auto enum_def = GetEnum(type);
      return GenerateImplNs(enum_def->underlying_type());
    } else {
      return "{{ ERROR GenerateImplNs }}\n";
    }
  }

  std::string GenerateType(const r::Type *type, const std::string &in_ns,
                           bool intf = true) {
    auto ns = intf ? GenerateIntfNs(type, in_ns) : GenerateImplNs(type);
    if (!ns.empty()) ns += ".";
    return ns + "t";
  }

  std::string GenerateReaderType(const r::Type *type, const std::string &in_ns,
                                 bool optional = false) {
    auto type_name = GenerateType(type, in_ns);
    if (IsImmediateType(type))
      return type_name + (optional ? " option" : "");
    else
      return "('b, " + type_name + ") " + RuntimeNS +
             (optional ? ".fbopt" : ".fb");
  }

  // types for builder arguments. Just uses tuple arguments for struct types,
  // just like the Struct.Vector definition does for builder_elt ....
  std::string GenerateBuilderType(const r::Type *type,
                                  const std::string &in_ns) {
    auto type_name = GenerateType(type, in_ns);
    if (IsImmediateType(type) || IsStruct(type))
      return type_name;
    else
      return type_name + " " + RuntimeNS + ".wip";
  }

  // need 3 forms:
  // - unexpanded, MyStruct.t (implmented with GenerateType?); for builder args
  // - 1 level: if top level type it's a struct, expanded to tuple; for mli
  // definition
  // - full: expand structs recursively, enums replaced with underlying type;
  // for ml definition
  std::string StructReprArgTypes(const r::Object *object,
                                 const std::string &in_ns, bool intf = true) {
    bool first = true;
    std::string res = "(";
    ForAllFields(object, /*reverse=*/false, [&](const r::Field *field) {
      if (field->type()->base_type() == r::Array) {
        auto elem_type = field->type()->element();
        std::string elem_repr;
        if (IsScalar(elem_type)) {
          auto ns = intf ? GenerateIntfNs(field->type(), in_ns, true)
                         : GenerateImplNs(field->type(), true);
          elem_repr = ns + ".t";
        } else if (elem_type == r::Obj) {
          auto obj = GetObject(field->type(), true);
          if (intf) {
            elem_repr = GenerateIntfNs(field->type(), in_ns, true) + ".t";
          } else {
            elem_repr = StructReprArgTypes(obj, in_ns, false);
          }
        }
        res += (first ? "" : " * ") + elem_repr + " array";
      } else if (IsScalar(field->type()->base_type()) ||
          (IsStruct(field->type()) && intf)) {
        res += (first ? "" : " * ") + GenerateType(field->type(), in_ns, intf);
      } else if (IsStruct(field->type()) && !intf) {
        res += (first ? "" : " * ") +
               StructReprArgTypes(GetObject(field->type()), in_ns, intf);
      } else {
        res += "{{ ERROR StructReprArgTypes }}\n";
      }
      first = false;
    });
    return res + ")";
  }

  std::string StructSetFn(const r::Type *type) {
    const r::BaseType base_type = type->base_type();
    if (IsScalar(base_type)) {
      return RuntimeNS + ".Builder.set_scalar T" +
             r::EnumNameBaseType(base_type);
    } else if (base_type == r::Obj) {
      auto object = GetObject(type);
      if (object->is_struct()) return StructSetIdent(object);
    }
    return "{{ ERROR StructReprArg }}";
  }

  std::string GenerateUnionArgTypes(const r::Type *type,
                                    const std::string &in_ns) {
    std::string args = "";
    ForAllEnumValues(GetEnum(type), [&](const reflection::EnumVal *e) {
      if (e->union_type()->base_type() == r::None) {
        args += " ?none:'a ->";
      } else {
        args += " ?" + namer_.Variable(e->name()->str()) + ":(" +
                GenerateReaderType(e->union_type(), in_ns) + " -> 'a) ->";
      }
    });
    args += " default:(" + GenerateType(type, in_ns) + " -> 'a) ->";
    return args;
  }

  // TODO(dmitrig): this uses the literal name, which may include long
  // namespaces. Check what other generators do
  std::string GenerateUnionArgs(const r::Type *type) {
    std::string args = "";
    ForAllEnumValues(GetEnum(type), [&](const r::EnumVal *e) {
      args += " ?" + namer_.Variable(e->name()->str());
    });
    args += " ~default";
    return args;
  }

  std::string GenerateDocumentation(
      const flatbuffers::Vector<flatbuffers::Offset<flatbuffers::String>> *docs,
      int level, std::string extra = "") const {
    const std::string indent = Indent(level);
    std::string res = "";
    if (!docs) return res;

    std::string body_indent = indent + "    ";
    bool first = true;
    res += indent + "(** ";
    flatbuffers::ForAllDocumentation(
        docs, [&](const flatbuffers::String *str) {
          if (!first) res += body_indent;
          res += str->str() + "\n";
          first = false;
        });
    if (!extra.empty()) res += "\n" + body_indent + extra + "\n";
    res += indent + "*)\n";
    return res;
  }

  std::string Indent(int level, int spaces = 2) const {
    return std::string(spaces * level, ' ');
  }

  struct Node {
    std::string name;
    int level = 0;
    const r::Enum *enum_def = nullptr;
    const r::Object *object = nullptr;
    std::map<std::string, size_t> children;
    std::vector<Node> nodes;
  };

  std::vector<std::string> NamespaceComponents(const std::string &full_name) {
    std::vector<std::string> labels;
    {
      std::string ns, rest = full_name;
      while (rest != "") {
        labels.push_back(namer_.Denamespace(rest, ns));
        rest = ns;
      }
    }

    // TODO(dmitrig): cleanup
    std::reverse(labels.begin(), labels.end());
    return labels;
  }

  std::vector<std::string> NamespaceRelComponents(const std::string &full_name,
                                                  const std::string &ns) {
    std::vector<std::string> name_components = NamespaceComponents(full_name);
    std::vector<std::string> ns_components = NamespaceComponents(ns);

    auto namei = name_components.begin();
    auto nsi = ns_components.begin();
    while (namei != name_components.end() && nsi != ns_components.end() &&
           *namei == *nsi) {
      namei++;
      nsi++;
    }

    return std::vector<std::string>(namei, name_components.end());
  }

  Node *AddNode(const std::string &full_name) {
    std::vector<std::string> labels = NamespaceComponents(full_name);

    // TODO(dmitrig): simplify
    Node *current = &root_node_;
    int level = 0;
    for (const auto &l : labels) {
      if (current->children.find(l) == current->children.end()) {
        Node n;
        n.name = namer_.Namespace(l);
        n.level = level;
        current->nodes.push_back(n);
        current->children[l] = current->nodes.size() - 1;
      }
      level++;
      current = &current->nodes.at(current->children[l]);
    }

    return current;
  }

  void EmitCodeHeader(std::string &intf, std::string &impl) {
    std::string header =
        "(** Automatically generated by the FlatBuffers compiler\n\n";
    auto root_table = schema_->root_table();
    if (root_table != nullptr) {
      std::string root_type = root_table->name()->str();
      std::string root_file = root_table->declaration_file()->str();
      header += "    root type: " + root_type + " (" + root_file + ")\n";
    }
    header += "    flatc version: " + flatc_version_ + "\n*)\n\n";

    intf += header;
    impl += header;

    // runtime library
    impl += "[@@@warning \"-32\"]\n\n";  // turn off unused-value-declaration

    impl += "module Rt = Flatbuffers.Runtime\n\n";

    intf += "module Rt : Flatbuffers.Runtime.Intf\n\n";
  }

  // Collect all FQ object/enum names contained within a node (recursively)
  void CollectNodeNames(const Node &node,
                        std::map<std::string, size_t> &map,
                        size_t child_idx) {
    if (node.object) map[node.object->name()->str()] = child_idx;
    if (node.enum_def) map[node.enum_def->name()->str()] = child_idx;
    for (const auto &child : node.nodes) {
      CollectNodeNames(child, map, child_idx);
    }
  }

  // Collect all objects contained within a node (recursively)
  void CollectNodeObjects(const Node &node,
                          std::vector<const r::Object *> &objects) {
    if (node.object) objects.push_back(node.object);
    for (const auto &child : node.nodes) {
      CollectNodeObjects(child, objects);
    }
  }

  // Return children indices in topological order (dependencies first)
  std::vector<size_t> SortedChildIndices(const Node &parent) {
    size_t n = parent.nodes.size();
    if (n <= 1) {
      std::vector<size_t> result;
      for (size_t i = 0; i < n; i++) result.push_back(i);
      return result;
    }

    // Map: FQ name -> which child index contains it
    std::map<std::string, size_t> name_to_child;
    for (size_t i = 0; i < n; i++) {
      CollectNodeNames(parent.nodes[i], name_to_child, i);
    }

    // Build dependency graph among children.
    // Recursively collect all objects inside each child and aggregate deps.
    std::vector<std::set<size_t>> deps(n);
    for (size_t i = 0; i < n; i++) {
      std::vector<const r::Object *> objects;
      CollectNodeObjects(parent.nodes[i], objects);
      for (auto obj : objects) {
        for (const auto &dep : GetDependencies(obj)) {
          auto it = name_to_child.find(dep);
          if (it != name_to_child.end() && it->second != i) {
            deps[i].insert(it->second);
          }
        }
      }
    }

    // Kahn's topological sort
    std::vector<std::vector<size_t>> rdeps(n);
    std::vector<size_t> in_degree(n, 0);
    for (size_t i = 0; i < n; i++) {
      in_degree[i] = deps[i].size();
      for (auto j : deps[i]) {
        rdeps[j].push_back(i);
      }
    }

    std::queue<size_t> queue;
    for (size_t i = 0; i < n; i++) {
      if (in_degree[i] == 0) queue.push(i);
    }

    std::vector<size_t> result;
    while (!queue.empty()) {
      size_t v = queue.front(); queue.pop();
      result.push_back(v);
      for (auto u : rdeps[v]) {
        if (--in_degree[u] == 0) queue.push(u);
      }
    }

    // Handle cycles: append remaining nodes
    if (result.size() < n) {
      std::set<size_t> in_result(result.begin(), result.end());
      for (size_t i = 0; i < n; i++) {
        if (!in_result.count(i)) result.push_back(i);
      }
    }

    return result;
  }

  std::string GenerateModuleDoc(const Node &node) {
    std::string full_name;
    std::string declaring_file;

    if (node.enum_def) {
      full_name = node.enum_def->name()->str();
      declaring_file = node.enum_def->declaration_file()->str();
    } else if (node.object) {
      full_name = node.object->name()->str();
      declaring_file = node.object->declaration_file()->str();
    }

    std::string header;
    if (node.enum_def || node.object)
      header = " " + full_name + " (" + declaring_file + ")";
    if (node.enum_def)
      header = (node.enum_def->is_union() ? "Union" : "Enum") + header;
    else if (node.object)
      header = (node.object->is_struct() ? "Struct" : "Table") + header;

    std::string doc;
    if (node.enum_def) {
      doc = GenerateDocumentation(node.enum_def->documentation(), node.level,
                                  /*extra=*/header);
    } else if (node.object) {
      doc = GenerateDocumentation(node.object->documentation(), node.level,
                                  /*extra=*/header);
    }

    if (doc.empty() && !header.empty())
      doc = Indent(node.level) + "(* " + header + " *)\n";

    return doc;
  }

  // Strip 'private' from type declarations for .ml inline sig.
  // In module rec, the inline sig seals the module, so enum types with
  // 'type t = private Foo' would prevent using Foo values as t.
  // The .mli still has 'private' for external type safety.
  static std::string StripPrivate(const std::string &sig) {
    std::string result = sig;
    const std::string from = "type t = private ";
    const std::string to = "type t = ";
    size_t pos = 0;
    while ((pos = result.find(from, pos)) != std::string::npos) {
      result.replace(pos, from.length(), to);
      pos += to.length();
    }
    return result;
  }

  // Returns {sig_content, impl_content} for a module's body
  std::pair<std::string, std::string> EmitModuleContent(const Node &node) {
    std::string sig, impl;

    // Generate this node's own content (leaf: object or enum)
    if (node.enum_def) {
      GenerateEnum(node.enum_def, node.level + 1, sig, impl);
    } else if (node.object) {
      GenerateObject(node.object, node.level + 1, sig, impl);
    }

    // Generate children as module rec group
    if (!node.nodes.empty()) {
      auto order = SortedChildIndices(node);
      bool first = true;
      for (auto idx : order) {
        const auto &child = node.nodes[idx];
        auto child_content = EmitModuleContent(child);
        std::string doc = GenerateModuleDoc(child);
        std::string indent = Indent(child.level);

        std::string comment;
        if (!(child.enum_def || child.object))
          comment = " (* " + child.name + " *)";

        std::string kw = first ? "module rec " : "and ";

        // .mli: module rec X : sig ... end (with private)
        sig += (first ? "" : "\n") + doc + indent + kw + child.name +
               " : sig\n" + child_content.first + indent + "end" +
               comment + "\n";

        // .ml: module rec X : sig ... end = struct ... end
        // Strip 'private' from inline sig so types are transparent
        // within the module rec group
        std::string ml_sig = StripPrivate(child_content.first);
        impl += (first ? "" : "\n") + doc + indent + kw + child.name +
                " : sig\n" + ml_sig + indent +
                "end = struct\n" + child_content.second + indent + "end" +
                comment + "\n";

        first = false;
      }
    }

    return {sig, impl};
  }

  void EmitCode(std::string &intf, std::string &impl) {
    auto content = EmitModuleContent(root_node_);
    intf += content.first;
    impl += content.second;
  }

  void WriteFiles(const std::string &intf, const std::string &impl) {
    // TODO(dmitrig): manually naming output based on root here. Should suffix
    // _generated?
    auto root_table = schema_->root_table();
    std::string root_file =
        root_table ? root_table->declaration_file()->str() : "flatc_output";
    const std::string impl_file =
        namer_.File(StripExtension(StripPath(root_file)));
    const std::string intf_file = impl_file + "i";

    const std::string impl_path = options_.output_path + impl_file;
    const std::string intf_path = options_.output_path + intf_file;
    options_.file_saver->SaveFile(intf_path.c_str(), intf, false);
    options_.file_saver->SaveFile(impl_path.c_str(), impl, false);

    root_node_ = {};
  }

  Node root_node_;
  int ident_counter_;
  std::map<std::string, std::string> enum_reader_idents_;
  const std::string flatc_version_;
  const BfbsNamer namer_;
  CodeGenOptions options_;
};
}  // namespace

std::unique_ptr<CodeGenerator> NewOCamlBfbsGenerator(
    const std::string &flatc_version) {
  return std::unique_ptr<OCamlBfbsGenerator>(
      new OCamlBfbsGenerator(flatc_version));
}

}  // namespace flatbuffers
