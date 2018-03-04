Module Module1

	Private sum As Double = 0.0

	Private sumString As String = ""

	Private sumResidue As Double = 0

	Private commonDigits As Int16 = 0

	Private commonPart As String = ""

	Private numberOfZero As Int64

	Private sumBefore As String = "0"

	Public Sub Main()

		'Dim childThread As Threading.Thread = New Threading.Thread(New Threading.ThreadStart(AddressOf doCalculation))

		'childThread.Start()

		'doCalculation()

		'Dim a As String = Console.ReadLine()

		'Dim b As String = Console.ReadLine()

		'Console.WriteLine(add(a, b))

		Do

			'Dim a As Int64 = Int64.MaxValue

			'Console.WriteLine(a + 1)

			Console.WriteLine(getNumberOfZero(Console.ReadLine()))

		Loop

		Console.ReadKey()

	End Sub

	Private Sub doComputation()

		Dim iterationIndex As Int64 = 0

		Dim sumAfter As String = "0"

		Dim termTrial As String = "0"

		Dim termApproximate As String = "0"

		numberOfZero = getNumberOfZero(termTrial)

		Do

			Try

				termTrial = 8 / ((1 + 4 * iterationIndex) * (3 + 4 * iterationIndex))

			Catch ex As Exception

				'Try Customized Add(,) Method

			End Try

			commonPart = Mid(sumBefore, 1, numberOfZero)

			sumString &= commonPart

			'Write Main Procedure Here

			iterationIndex += 1

		Loop

	End Sub

	Private Function term(i As Int64) As Double

		term = 8 / ((1 + 4 * i) * (3 + 4 * i))

		term *= powerOfTen()

	End Function

	Private Function getNumberOfZero(number As String)

		'SPECIAL NOTICE: INCLUDES "."

		For i As Int64 = 1 To Len(number)

			If Mid(number, i, 1) <> "0" AndAlso Mid(number, i, 1) <> "." Then

				Return i - 1

			End If

		Next

		Return 0

	End Function

	Private Function getCommonPart(number As String) As String

		Return Mid(number, 1, getNumberOfZero(number))

	End Function

	'Method getResidue Reload +1 (New)
	Private Function getResidue(number As String) As String

		Return Mid(number, Len(numberOfZero), Len(number) - Len(numberOfZero))

	End Function

	Private Function powerOfTen() As Int64

		If commonDigits >= 2 Then

			Return 10 ^ (commonDigits - 2)

		Else

			Return 1

		End If

	End Function

#Region "Old Functions"

	Private Sub doCalculation()

		Dim iterationIndex As Int64 = 0
		Dim sumCopy(99) As Double
		Dim sumCopyIndex As Int16 = 0

		Do
			sumCopy(0) = sum

			sumCopyIndex += 1

			sumCopy(sumCopyIndex) = sumCopy(sumCopyIndex - 1)

			For i As Integer = 100000 * iterationIndex To 100000 * (iterationIndex + 1) - 1

				sumCopy(sumCopyIndex) = sumCopy(sumCopyIndex) + term(i)

			Next

			If sumCopyIndex = 99 Then

				sumString &= getCommonPart(sumCopy)

				Console.Write(sumString)
				'Threading.Thread.Sleep(100)

				sumResidue = getResidue(sumCopy)

				sum = sumResidue

				'Finallize
				For Each item In sumCopy

					item = 0

				Next

				sumCopyIndex = 0

				commonDigits = 0

			End If

			iterationIndex += 1

		Loop

	End Sub

	Private Function getCommonPart(sumCopys() As Double) As String

		Dim commonPart As String = ""

		Dim ProbablyMoreSameDigits As Boolean = True

		For i As Integer = 1 To Len(sumCopys(0))

			Dim digitsArray(sumCopys.Length - 1) As String

			For j As Integer = 0 To sumCopys.Length - 1

				digitsArray(j) = Mid(sumCopys(j), i, 1)

			Next

			If isIdentical(digitsArray) And ProbablyMoreSameDigits Then

				commonPart &= digitsArray(0)

				commonDigits += 1

			End If

			If isIdentical(digitsArray) = False Then

				ProbablyMoreSameDigits = False

			End If

		Next

		If Mid(commonPart, 1, 2) = "0." Then

			Return commonPart * 10

		Else

			Return commonPart

		End If

	End Function

	Private Function isIdentical(digits As String()) As Boolean

		For i As Integer = 1 To digits.Length - 1

			If digits(i) <> digits(0) Then

				Return False

			End If

		Next

		Return True

	End Function

	Private Function getResidue(sumCopys() As Double) As Double

		If commonDigits = 0 Then

			Return sumCopys(sumCopys.Length - 1)

		ElseIf commonDigits = 2 Then

			Return "0." & Mid(sumCopys(sumCopys.Length - 1), 3, 7)

		Else

			Return "0." & Mid(sumCopys(sumCopys.Length - 1), commonDigits + 1, 7)

		End If

	End Function

#Region "Add ExtraLong Decimals"

	Private Function add(number1 As String, number2 As String) As String

		Dim number1_Integer As String = getInteger(number1)

		Dim number2_Integer As String = getInteger(number2)

		Dim lengthOfIntegerArray As Int64 = Math.Truncate(Math.Max(Len(number1_Integer), Len(number2_Integer)) / 10)

		Dim array1_Integer(lengthOfIntegerArray) As String

		Dim array2_Integer(lengthOfIntegerArray) As String

		Dim array3_Integer(lengthOfIntegerArray) As String

		'Integer Part

		For i As Int64 = 0 To lengthOfIntegerArray

			array1_Integer(i) = Mid(number1_Integer, 1 + 10 * i, 10)

			array2_Integer(i) = Mid(number2_Integer, 1 + 10 * i, 10)

		Next

		For i As Int64 = 0 To lengthOfIntegerArray

			array3_Integer(i) += Convert.ToInt64(array1_Integer(i)) + Convert.ToInt64(array2_Integer(i))

			If Len(array3_Integer(i)) = 11 And i <> lengthOfIntegerArray Then

				array3_Integer(i + 1) = Convert.ToInt64(array3_Integer(i + 1)) + 1

				array3_Integer(i) = Right(array3_Integer(i), 10)

			ElseIf Len(array3_Integer(i)) = 11 And i = lengthOfIntegerArray Then

				ReDim array3_Integer(lengthOfIntegerArray + 1)

				array3_Integer(lengthOfIntegerArray + 1) = 1

			End If

		Next

		'Decimal Part

		Dim number1_Decimal As String = getDecimal(number1)

		Dim number2_Decimal As String = getDecimal(number2)

		Dim lengthOfDecimalArray As Int64 = Math.Truncate(Math.Max(Len(number1_Decimal), Len(number2_Decimal)) / 10)

		Dim array1_Decimal(lengthOfDecimalArray) As String

		Dim array2_Decimal(lengthOfDecimalArray) As String

		Dim array3_Decimal(lengthOfDecimalArray) As String

		For i As Int64 = 0 To lengthOfDecimalArray

			array1_Decimal(i) = Mid(number1_Decimal, 1 + 10 * i, 10)

			array2_Decimal(i) = Mid(number2_Decimal, 1 + 10 * i, 10)

		Next

		array1_Decimal(lengthOfDecimalArray) = Convert.ToInt64(array1_Decimal(lengthOfDecimalArray)) * (10 ^ (10 - Len(array1_Decimal(lengthOfDecimalArray))))

		array2_Decimal(lengthOfDecimalArray) = Convert.ToInt64(array1_Decimal(lengthOfDecimalArray)) * (10 ^ (10 - Len(array2_Decimal(lengthOfDecimalArray))))


		For i As Int64 = 0 To lengthOfDecimalArray

			array3_Decimal(i) += Convert.ToInt64(array1_Decimal(i)) + Convert.ToInt64(array2_Decimal(i))

			If Len(array3_Decimal(i)) = 11 And i <> 0 Then

				array3_Decimal(i - 1) = Convert.ToInt64(array3_Decimal(i - 1)) + 1

				array3_Decimal(i) = Right(array3_Decimal(i), 10)

			ElseIf Len(array3_Decimal(i)) = 11 And i = lengthOfDecimalArray Then

				ReDim array3_Decimal(lengthOfDecimalArray + 1)

				array3_Decimal(lengthOfDecimalArray + 1) = 1

			End If

		Next

		add = ""

		For i As Int64 = array3_Integer.Length - 1 To 0 Step -1

			add &= array3_Integer(i)

		Next

		If array3_Decimal(0) <> "" And (getDecimal(number1) <> 0 And getDecimal(number2) <> 0) Then

			add &= "."

			For i As Int64 = 0 To array3_Decimal.Length - 1

				add &= array3_Decimal(i)

			Next

		End If

	End Function

	Private Function add1(number As String) As String

		Try

			Return Convert.ToInt64(number) + 1

		Catch ex As OverflowException

			Mid(number, Len(number), 1) = Convert.ToInt64(Mid(number, Len(number), 1)) + 1

			Return Mid(number, 1, Len(number) - 1) & Mid(number, Len(number), 1)

		End Try

	End Function

	Private Function getInteger(number As String) As String

		If locateDecimalPoint(number) = 0 Then

			Return number

		Else

			Return Mid(number, 1, locateDecimalPoint(number) - 1)

		End If

	End Function

	Private Function getDecimal(number As String) As String

		If locateDecimalPoint(number) = 0 Then

			Return 0

		Else

			Return Mid(number, locateDecimalPoint(number) + 1)

		End If

	End Function

	Private Function locateDecimalPoint(number As String) As Int64

		For i As Int64 = 2 To Len(number)

			If Mid(number, i, 1) = "." Then

				Return i

			End If

		Next

		Return 0

	End Function

#End Region

#End Region

End Module
