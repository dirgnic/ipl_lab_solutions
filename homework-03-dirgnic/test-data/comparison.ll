; ModuleID = 'comparison'
source_filename = "comparison.baelv"

define i64 @myMain() {
myEntrypoint:
  %y = icmp slt i64 15, 15
  %z = and i1 %y, true
  ret i1 %z
}
