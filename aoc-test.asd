(defsystem "aoc-test"
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("aoc-test/all"))

(register-system-packages "aoc-test/all" '("aoc-test"))
