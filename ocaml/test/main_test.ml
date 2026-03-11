let () = Alcotest.run "Flatbuffers" [ "Monster_test", Monster_test.test_cases
                                    ; "Arrays_test", Arrays_test.test_cases
                                    ; "Offset64_test", Offset64_test.test_cases ]
