; ModuleID = 'eq'
source_filename = "eq.baelv"

define i64 @myMain() {
myEntrypoint:
  %x0 = icmp eq i64 5, 5
  ret i1 %x0
}
