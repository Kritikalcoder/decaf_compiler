
@A = global [10 x i32] zeroinitializer
@size = global i32 0
@0 = private unnamed_addr constant [3 x i8] c"%d\00"

define i32 @arraySum() {
entry:
  %i = alloca i32, align 4
  %sum = alloca i32, align 4
  store i32 0, i32* %sum, align 4
  store i32 0, i32* %i, align 4
  %.pre = load i32, i32* @size, align 4
  br label %loop

loop:                                             ; preds = %loop, %entry
  %0 = phi i32 [ %addEqualToTmp, %loop ], [ 0, %entry ]
  %i2 = phi i32 [ %NextVal, %loop ], [ 0, %entry ]
  %1 = sext i32 %i2 to i64
  %A_Index = getelementptr [10 x i32], [10 x i32]* @A, i64 0, i64 %1
  %2 = load i32, i32* %A_Index, align 4
  %addEqualToTmp = add i32 %0, %2
  store i32 %addEqualToTmp, i32* %sum, align 4
  %NextVal = add i32 %i2, 1
  store i32 %NextVal, i32* %i, align 4
  %loopcondition = icmp slt i32 %NextVal, %.pre
  br i1 %loopcondition, label %loop, label %afterloop

afterloop:                                        ; preds = %loop
  ret i32 %addEqualToTmp
  ret i32 %addEqualToTmp
}

define void @main() {
entry:
  %i = alloca i32, align 4
  store i32 4, i32* @size, align 4
  store i32 0, i32* %i, align 4
  br label %loop

loop:                                             ; preds = %loop, %entry
  %0 = phi i32 [ %NextVal, %loop ], [ 0, %entry ]
  %1 = sext i32 %0 to i64
  %A_Index = getelementptr [10 x i32], [10 x i32]* @A, i64 0, i64 %1
  store i32 %0, i32* %A_Index, align 4
  %NextVal = add i32 %0, 1
  store i32 %NextVal, i32* %i, align 4
  %loopcondition = icmp slt i32 %NextVal, 4
  br i1 %loopcondition, label %loop, label %afterloop

afterloop:                                        ; preds = %loop
  %2 = call i32 @arraySum()
  %3 = call i32 @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @0, i64 0, i64 0), i32 %2)
  ret void
}

declare i32 @printf(i8*, i32)
