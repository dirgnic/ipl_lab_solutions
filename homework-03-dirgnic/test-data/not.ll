; ModuleID = 'not'
source_filename = "not.baelv"

define i64 @myMain() {
myEntrypoint:
  %x0 = xor i1 true, true
  ret i1 %x0
}
