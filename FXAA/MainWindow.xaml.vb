Imports System.Numerics
Imports System.IO
Imports System.Windows.Media.Media3D

Class MainWindow
    Public ImageWidth As Integer = 800
    Public ImageHeight As Integer = 800

    Sub SaveBMP(ByRef imageData() As Byte, ByVal filename As String)
        Dim bmp_file_size As Integer = 53 + (ImageHeight * ImageWidth * 3)

        Dim bmp_f_header(14 - 1) As Byte
        bmp_f_header(0) = 66
        bmp_f_header(1) = 77
        bmp_f_header(2) = (bmp_file_size >> 0) And &HFF
        bmp_f_header(3) = (bmp_file_size >> 8) And &HFF
        bmp_f_header(4) = (bmp_file_size >> 16) And &HFF
        bmp_f_header(5) = (bmp_file_size >> 24) And &HFF
        bmp_f_header(10) = 54

        Dim bmp_i_header(40 - 1) As Byte
        bmp_i_header(0) = 40
        bmp_i_header(4) = (ImageWidth >> 0) And &HFF
        bmp_i_header(5) = (ImageWidth >> 8) And &HFF
        bmp_i_header(6) = (ImageWidth >> 16) And &HFF
        bmp_i_header(7) = (ImageWidth >> 24) And &HFF
        bmp_i_header(8) = (ImageHeight >> 0) And &HFF
        bmp_i_header(9) = (ImageHeight >> 8) And &HFF
        bmp_i_header(10) = (ImageHeight >> 16) And &HFF
        bmp_i_header(11) = (ImageHeight >> 24) And &HFF
        bmp_i_header(12) = 1
        bmp_i_header(14) = 24

        Dim bmp_pad As Integer = (4 - (ImageWidth * 3) Mod 4) Mod 4

        Dim f As New FileStream(filename, FileMode.Create)
        f.Write(bmp_f_header, 0, 14)
        f.Write(bmp_i_header, 0, 40)

        For i As Integer = 0 To (ImageHeight - 1)
            f.Write(imageData, ImageWidth * (ImageHeight - i - 1) * 3, ImageWidth * 3)
            For pad As Integer = 0 To (bmp_pad - 1)
                f.WriteByte(0)
            Next
        Next
        f.Close()
    End Sub

    Public Sub GetPixel(ByRef imageData() As Byte, ByVal x As Integer, ByVal y As Integer, ByRef r As Byte, ByRef g As Byte, ByRef b As Byte)
        If (x >= 0 And x < ImageWidth) And (y >= 0 And y < ImageHeight) Then
            Dim index As Integer = y * (ImageWidth * 3) + (x * 3)
            b = imageData(index + 0)
            g = imageData(index + 1)
            r = imageData(index + 2)
        End If
    End Sub
    Public Function GetPixelVector(ByRef imageData() As Byte, ByVal x As Integer, ByVal y As Integer) As Vector3D
        If (x >= 0 And x < ImageWidth) And (y >= 0 And y < ImageHeight) Then
            Dim index As Integer = y * (ImageWidth * 3) + (x * 3)
            Return New Vector3D(imageData(index + 2) / 255.0, imageData(index + 1) / 255.0, imageData(index + 0) / 255.0)
        Else
            Return New Vector3D(0, 0, 0)
        End If

    End Function
    Public Sub SetPixel(ByRef imageData() As Byte, ByVal x As Integer, ByVal y As Integer, ByVal r As Byte, ByVal g As Byte, ByVal b As Byte)
        Dim index As Integer = y * (ImageWidth * 3) + (x * 3)
        imageData(index + 0) = b
        imageData(index + 1) = g
        imageData(index + 2) = r
    End Sub
    Public Sub SetPixelVector(ByRef imageData() As Byte, ByVal x As Integer, ByVal y As Integer, ByVal rgb As Vector3D)
        Dim index As Integer = y * (ImageWidth * 3) + (x * 3)
        imageData(index + 0) = rgb.Z * 255.0
        imageData(index + 1) = rgb.Y * 255.0
        imageData(index + 2) = rgb.X * 255.0
    End Sub

    Public Function DrawStatic() As Byte()
        Dim imageData((ImageWidth * ImageHeight * 3) - 1) As Byte

        For x As Integer = 0 To ImageWidth - 1
            For y As Integer = 0 To ImageHeight - 1
                SetPixel(imageData, x, y, Rnd() * 255, Rnd() * 255, Rnd() * 255)
            Next
        Next

        Return imageData
    End Function

    Public Shared Function HSVToRGB(ByVal h As Double, ByVal s As Double, ByVal v As Double) As Vector3D
        Dim r As Double = 0, g As Double = 0, b As Double = 0

        If s = 0 Then
            r = v
            g = v
            b = v
        Else
            Dim i As Integer
            Dim f As Double, p As Double, q As Double, t As Double

            If h = 360 Then
                h = 0
            Else
                h = h / 60
            End If

            i = CInt(Math.Truncate(h))
            f = h - i

            p = v * (1.0 - s)
            q = v * (1.0 - (s * f))
            t = v * (1.0 - (s * (1.0 - f)))

            Select Case i
                Case 0
                    r = v
                    g = t
                    b = p
                    Exit Select

                Case 1
                    r = q
                    g = v
                    b = p
                    Exit Select

                Case 2
                    r = p
                    g = v
                    b = t
                    Exit Select

                Case 3
                    r = p
                    g = q
                    b = v
                    Exit Select

                Case 4
                    r = t
                    g = p
                    b = v
                    Exit Select
                Case Else

                    r = v
                    g = p
                    b = q
                    Exit Select

            End Select
        End If

        Return New Vector3D(r, g, b)
    End Function

    Public Function CalculateJulia(ByVal z As Complex, ByVal c As Complex, ByVal maxCalc As Integer)
        Dim zc As Complex = z
        For i As Integer = 0 To maxCalc - 1
            z = (z * z) + zc
            If z.Magnitude > 2.0 Then
                Return i
            End If
        Next
        Return maxCalc
    End Function
    Public Function DrawJulia() As Byte()
        Dim imageData((ImageWidth * ImageHeight * 3) - 1) As Byte

        'Dim c As New Complex(0.33, 0.55)
        Dim c As New Complex(0.279, 0.0)
        Dim center As New Complex(0, 0)
        Dim zoom As Double = 4.0
        Dim aspect As Double = CDbl(ImageWidth) / CDbl(ImageHeight)
        Dim maxCalc As Integer = 200

        Dim expZoom As Double = Math.Exp(zoom * Math.Log(1.1))

        For x As Integer = 0 To ImageWidth - 1
            For y As Integer = 0 To ImageHeight - 1
                Dim xNorm As Double = (x / (ImageWidth / 2.0)) - 1.0
                Dim yNorm As Double = (y / (ImageHeight / 2.0)) - 1.0

                Dim z As New Complex(xNorm * expZoom + center.Real, (yNorm * expZoom + center.Imaginary) / aspect)

                Dim time As Integer = CalculateJulia(z, c, maxCalc)
                'Dim b As Integer = (time / CDbl(maxCalc)) * 255
                'SetPixel(imageData, x, y, b, b, b)
                If time <> 0 Then
                    Dim color As Vector3D = HSVToRGB((time / CDbl(maxCalc)) * 360, 1.0, 0.2)
                    SetPixelVector(imageData, x, y, color)
                Else
                    SetPixelVector(imageData, x, y, New Vector3D(0, 0, 0))
                End If

            Next
        Next

        Return imageData
    End Function

    Public Function DrawCircle() As Byte()
        Dim imageData((ImageWidth * ImageHeight * 3) - 1) As Byte

        Dim cx As Integer = 400
        Dim cy As Integer = 400
        Dim r As Integer = 300

        Dim tx, ty As Integer
        Dim r2 As Integer = r * r
        Dim area As Integer = r2 << 2
        Dim rr As Integer = r << 1

        For i As Integer = 0 To area - 1
            tx = (i Mod rr) - r
            ty = Math.Truncate(i / rr) - r

            If (tx * tx + ty * ty <= r2) Then
                SetPixelVector(imageData, cx + tx, cy + ty, New Vector3D(1, 1, 1))
            End If
        Next

        Return imageData
    End Function

    Public Function FXAA(ByRef imageData() As Byte) As Byte()
        Dim fxaaData((ImageWidth * ImageHeight * 3) - 1) As Byte

        ' borrowed from https://github.com/mattdesl/glsl-fxaa/blob/master/fxaa.glsl

        Dim fxaaReduceMin As Double = (1.0 / 128.0)
        Dim fxaaReduceMul As Double = (1.0 / 8.0)
        Dim fxaaSpanMax As Double = (8.0)

        For x As Integer = 0 To ImageWidth - 1
            For y As Integer = 0 To ImageHeight - 1
                Dim rgbNW As Vector3D = GetPixelVector(imageData, x - 1, y + 1)
                Dim rgbNE As Vector3D = GetPixelVector(imageData, x + 1, y + 1)
                Dim rgbSW As Vector3D = GetPixelVector(imageData, x - 1, y - 1)
                Dim rgbSE As Vector3D = GetPixelVector(imageData, x + 1, y - 1)
                Dim rgbM As Vector3D = GetPixelVector(imageData, x, y)

                Dim luma As New Vector3D(0.299, 0.587, 0.114)

                Dim lumaNW As Double = Vector3D.DotProduct(rgbNW, luma)
                Dim lumaNE As Double = Vector3D.DotProduct(rgbNE, luma)
                Dim lumaSW As Double = Vector3D.DotProduct(rgbSW, luma)
                Dim lumaSE As Double = Vector3D.DotProduct(rgbSE, luma)
                Dim lumaM As Double = Vector3D.DotProduct(rgbM, luma)

                Dim lumaMin As Double = Math.Min(Math.Min(Math.Min(Math.Min(lumaNW, lumaNE), lumaSW), lumaSE), lumaM)
                Dim lumaMax As Double = Math.Max(Math.Max(Math.Max(Math.Max(lumaNW, lumaNE), lumaSW), lumaSE), lumaM)

                Dim dirX As Double = -((lumaNW + lumaNE) - (lumaSW + lumaSE))
                Dim dirY As Double = ((lumaNW + lumaSW) - (lumaNE + lumaSE))

                Dim dirReduce As Double = Math.Max((lumaNW + lumaNE + lumaSW + lumaSE) * (0.25 * fxaaReduceMul), fxaaReduceMin)

                Dim rcpDirMin As Double = 1.0 / (Math.Min(Math.Abs(dirX), Math.Abs(dirY)) + dirReduce)

                dirX = Math.Min(fxaaSpanMax, Math.Max(-fxaaSpanMax, dirX * rcpDirMin))
                dirY = Math.Min(fxaaSpanMax, Math.Max(-fxaaSpanMax, dirY * rcpDirMin))

                Dim rgbAa As Vector3D = GetPixelVector(imageData, x + dirX * (1.0 / 3.0 - 0.5), y + dirY * (1.0 / 3.0 - 0.5))
                Dim rgbAb As Vector3D = GetPixelVector(imageData, x + dirX * (2.0 / 3.0 - 0.5), y + dirY * (2.0 / 3.0 - 0.5))
                Dim rgbA As Vector3D = 0.5 * (rgbAa + rgbAb)

                Dim rgbBa As Vector3D = GetPixelVector(imageData, x + dirX * -0.5, y + dirY * -0.5)
                Dim rgbBb As Vector3D = GetPixelVector(imageData, x + dirX * 0.5, y + dirY * 0.5)
                Dim rgbB As Vector3D = rgbA * 0.5 + 0.25 * (rgbBa + rgbBb)

                Dim lumaB As Double = Vector3D.DotProduct(rgbB, luma)

                If ((lumaB < lumaMin) Or (lumaB > lumaMax)) Then
                    SetPixelVector(fxaaData, x, y, rgbA)
                Else
                    SetPixelVector(fxaaData, x, y, rgbB)
                End If
            Next
        Next



        Return fxaaData
    End Function

    Public Function Difference(ByRef imageData1() As Byte, ByVal imageData2() As Byte)
        Dim diffData((ImageWidth * ImageHeight * 3) - 1) As Byte
        For x As Integer = 0 To ImageWidth - 1
            For y As Integer = 0 To ImageHeight - 1
                Dim rgb1 As Vector3D = GetPixelVector(imageData1, x, y)
                Dim rgb2 As Vector3D = GetPixelVector(imageData2, x, y)

                Dim delta As Vector3D = rgb2 - rgb1
                Dim diff As Double = delta.Length / Math.Sqrt(3)

                SetPixelVector(diffData, x, y, New Vector3D(diff, diff, diff))
            Next
        Next

        Return diffData
    End Function

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.Windows.RoutedEventArgs) Handles Button1.Click
        Dim imageData() As Byte = DrawJulia()
        SaveBMP(imageData, "test.bmp")
        'Dim fxaaData() As Byte = FXAA(imageData)
        'SaveBMP(fxaaData, "fxaa.bmp")

        'Dim diffData() As Byte = Difference(imageData, fxaaData)
        'SaveBMP(diffData, "diff.bmp")
    End Sub
End Class
