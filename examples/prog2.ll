; ModuleID = 'prog2'
source_filename = "prog2"
target triple = "arm64-apple-macosx13.0.0"

@_exn = external constant i8*

declare i32 @printf(i8* %0)

declare i32 @posix_memalign(i8** %0, i32 %1, i32 %2)

declare i32 @fiber1(i32 %0, i8* %1, i8* %2, i8* %3)

declare void @dummy_personality()

declare void @continue(i8* %0)

declare i32 @perform(i32 %0)

define i32 @add2(i32 %x) personality void ()* @dummy_personality {
entry:
  %V5 = call i32 @perform(i32 0)
  %V6 = add i32 %x, 4
  br label %exit

exit:                                             ; preds = %entry
  ret i32 %V6

lpad:                                             ; No predecessors!
  %V0 = landingpad { i32, i8*, i8* }
          catch i32* bitcast (i8** @_exn to i32*)
  %V1 = extractvalue { i32, i8*, i8* } %V0, 0
  %V2 = extractvalue { i32, i8*, i8* } %V0, 1
  %V3 = extractvalue { i32, i8*, i8* } %V0, 2
  unreachable
}

define i32 @main() personality void ()* @dummy_personality {
entry:
  %V14 = alloca i8*, align 8
  %V15 = call i32 @posix_memalign(i8** %V14, i32 16, i32 16384)
  %V16 = load i8*, i8** %V14, align 8
  %V18 = invoke i32 @fiber1(i32 10, i8* %V16, i8* bitcast (i32 (i32)* @add2 to i8*), i8* blockaddress(@main, %lpad))
          to label %block_V13 unwind label %lpad

block_V13:                                        ; preds = %entry
  %V21 = alloca i8*, align 8
  %V22 = call i32 @posix_memalign(i8** %V21, i32 16, i32 16384)
  %V23 = load i8*, i8** %V21, align 8
  %V25 = invoke i32 @fiber1(i32 20, i8* %V23, i8* bitcast (i32 (i32)* @add2 to i8*), i8* blockaddress(@main, %lpad))
          to label %block_V20 unwind label %lpad

block_V20:                                        ; preds = %block_V13
  %V26 = add i32 %V25, %V18
  br label %exit

exit:                                             ; preds = %block_V20
  ret i32 %V26

lpad:                                             ; preds = %block_V13, %entry
  %V7 = landingpad { i32, i8*, i8* }
          catch i32* bitcast (i8** @_exn to i32*)
  %V8 = extractvalue { i32, i8*, i8* } %V7, 0
  %V9 = extractvalue { i32, i8*, i8* } %V7, 1
  %V10 = extractvalue { i32, i8*, i8* } %V7, 2
  call void @continue(i8* %V10) #3
  ret i32 0
}

attributes #3 = { nounwind }
