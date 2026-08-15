let () =
  Alcotest.run
    "Flatbuffers"
    [ "Monster_test", Monster_test.test_cases
    ; "Arrays_test", Arrays_test.test_cases
    ; "Builder_test", Builder_test.test_cases
    ; "Offset64_test", Offset64_test.test_cases
    ; "String_union_test", String_union_test.test_cases
    ; "Union_vector_test", Union_vector_test.test_cases
    ; "Casing_test", Casing_test.test_cases
    ; "Verifier_test", Verifier_test.test_cases
    ; "Flexbuffers_test", Flexbuffers_test.test_cases
    ; "Util_test", Util_test.test_cases
    ]
;;
