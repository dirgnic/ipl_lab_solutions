; ModuleID = 'mismatch'
source_filename = "mismatch.baelv"

define i64 @myMain() {
myEntrypoint:
  %x = and i1 true, false
  %y = add i64 123, i1 %x
  %z = add i1 %x, i64 %y
  ret i1 %z
}
