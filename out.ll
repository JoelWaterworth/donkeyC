; ModuleID = 'main'
source_filename = "main"

@msg = private constant [12 x i8] c"hello world\00"

define i32 @main() {
entry:
  %0 = call i32 @puts(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @msg, i32 0, i32 0))
  ret i32 0
}

declare i32 @puts(i8*)