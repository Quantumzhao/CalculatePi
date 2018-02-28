Module Module1

	Private sum As Double = 0.1
	Private sumString As String = ""
	Private sumResidue As Double = 0
	Private commonDigits As Int16 = 0

	Public Sub Main()

		'Dim childThread As Threading.Thread = New Threading.Thread(New Threading.ThreadStart(AddressOf doCalculation))

		'childThread.Start()

		doCalculation()

		Console.ReadKey()

	End Sub

	Private Sub doCalculation()

		Dim iterationIndex As Int64 = 0
		Dim sumCopy(19) As Double
		Dim sumCopyIndex As Int16 = 0

		Do
			sumCopy(0) = sum

			sumCopyIndex += 1

			sumCopy(sumCopyIndex) = sumCopy(sumCopyIndex - 1) + term(iterationIndex)


			If sumCopyIndex = 19 Then

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

	Private Function term(i As Int64) As Double

		term = 8 / ((1 + 4 * i) * (3 + 4 * i))

		term *= powerOfTen()

	End Function

	Private Function powerOfTen() As Int64

		If commonDigits >= 2 Then

			Return 10 ^ (commonDigits - 2)

		Else

			Return 1

		End If

	End Function

	Private Function getCommonPart(sumCopys() As Double) As String

		Dim commonPart As String = ""

		Dim ProbablyMoreSameDigits As Boolean = False

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

End Module
