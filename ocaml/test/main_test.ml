let () = Alcotest.run "Flatbuffers" [ "Monster_test", Monster_test.test_cases
                                    ; "Arrays_test", Arrays_test.test_cases
                                    ; "Offset64_test", Offset64_test.test_cases
                                    ; "String_union_test", String_union_test.test_cases
                                    ; "Casing_test", Casing_test.test_cases ]
