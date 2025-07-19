Option Explicit On
Imports System.IO
Imports System.Xml


Public Class form1
    Dim asignadolabel(1000) As Boolean
    Dim xdelpunto(5200) As Integer
    Dim ydelpunto(5200) As Integer
    Dim xdelpuntoInicial(5200) As Integer
    Dim ydelpuntoInicial(5200) As Integer
    Dim lblmoto(1000) As Label
    Dim divisiones As Integer
    Dim TiempoultimavueltaTomada(1000) As Single
    Dim Tiempodeultimavueltatratada(1000) As Single
    Dim HoraDePasoPormeta(1000) As Double
    Dim HoraDeSalidaPit(1000) As Double
    Dim HoraDePasoPorSector1(1000) As Double
    Dim HoraDePasoPorSector2(1000) As Double
    Dim HoraDePasoPorSector3(1000) As Double
    Dim HoraDelDiaActual(1000) As String
    Dim HoraDelDiaAnterior(1000) As String
    Dim tiempo_entre_marcas(1000) As Single
    Dim tiempo_de_vuelta(1000) As Single
    Dim tiemposector1(1000) As Double
    Dim tiemposector2(1000) As Double
    Dim tiemposector3(1000) As Double
    Dim tiemposector4(1000) As Double
    Dim marker(1000) As String
    Dim ultimatimeline(1000) As String
    Dim posicion(1000) As Integer
    Dim vueltas(1000) As Integer
    Dim division_asignada(5200) As Integer
    Dim corredor_1 As Integer
    Dim corredor_2 As Integer
    Dim CorredorCabezadeCarrera As Integer
    Dim enpista(1000) As Boolean
    Dim indesadoElNumero(1000) As Boolean
    Dim numeroDelIndice(1000) As String
    Dim indice As Integer
    Dim ultimavueltatratada(1000) As Integer
    Dim archivodatosxml As String
    Dim sectores As Integer
    ' Dim diferenciademarcasensector1(1000) As Integer
    ' Dim diferenciademarcasensector2(1000) As Integer


    Dim conparciales As Boolean = False
    'conparciales=false para cuando no hay parciales y no pasa por estos if     conparciales=true  para cuando hay parciales 
    Dim puntosector1 As Integer
    Dim puntosector2 As Integer
    Dim puntosector3 As Integer
    Dim puntosector4 As Integer

    Dim h As Integer

    'ESTOS DATOS SON PARA EL CIRCUITO DE ALBACETE
    'Private Const puntosector1 As Integer = 5000 / 89.794 * 33.68 ' 1000   '1873 '40.3%  proporcion del primer parcial sobre el tiempo total
    'Private Const puntosector2 As Integer = 5000 / 89.794 * 58.25  '     '1675 + 2015         '33.5%  proporcion del segundo parcial sobre el tiempo total.
    'Private Const puntosector3 As Integer = 5000 '/ 89.794 * 69.388 '3858      '26.2%  proporcion del tercer parcial sobre el tiempo total.
    'Private Const puntosector4 As Integer = 5000        'Para el Circuito de Talavera con 4 sectores(3 Puntos intermedios de cronometraje), los 4 sectores son iguales 



    'Private Const puntosector1 As Integer = 1898   'CONSTANTES PARA EL SIMULADOR DE ORBITS
    'Private Const puntosector2 As Integer = 3254
    'Private Const puntosector3 As Integer = 5000 '- 1
    'Private Const puntosector4 As Integer = 5000


    'Private Const puntosector1 As Integer = 1254        'CONSTANTES PARA EL  CIRCUITO DE TALAVERA con TRES parciales
    'Private Const puntosector2 As Integer = 3083
    'Private Const puntosector3 As Integer = 5000 '- 1
    'Private Const puntosector4 As Integer = 5000

    'Private Const puntosector1 As Integer = 1298       'CONSTANTES PARA EL  CIRCUITO DE TALAVERA con CUATRO parciales
    'Private Const puntosector2 As Integer = 2545
    'Private Const puntosector3 As Integer = 3641        '- 1
    'Private Const puntosector4 As Integer = 5000



    Dim porcentajesector1 As Single '= 25 / 100      '40.3%  que corresponde al porcentaje del primer sector, para cirduito de Albacete
    Dim porcentajesector2 As Single '= 25 / 100      '33.5%  que corresponde al porcentaje del segundo sector
    Dim porcentajesector3 As Single '= 25 / 100      '26.2%  que corresponde al porcentaje del tercer sector ''
    Dim porcentajesector4 As Single





    Private Sub Grabar_Coord_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles Grabar_Coord.Click
        Dim i As Integer
        'Abre el cuadro de dialogo comun Guardar
        ' para obtener la ruta y el nombre del archivo
        SaveFileDialog1.ShowDialog()
        Try
            'Crea un flujo hacia el archivo
            Dim fs As New FileStream(SaveFileDialog1.FileName, FileMode.CreateNew, FileAccess.Write, FileShare.Write)
            'Escribe el contenido de un control RichTexbox en el archivo
            Dim texto As New IO.StreamWriter(fs)
            For i = 1 To divisiones
                texto.Write(i)
                texto.WriteLine()
                texto.Write(xdelpunto(i))
                texto.WriteLine()
                texto.Write(ydelpunto(i))
                texto.WriteLine()
            Next i
            texto.Close()
            fs.Close()
        Catch ex As Exception
            MessageBox.Show(ex.Message)
            'fs.close()
        End Try

    End Sub

    Private Sub comenzar_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles comenzar.Click

        'Pregunta por el nuemro de prciales.
        'sectores = Val(InputBox("Numero de sectores para el Circuito de Talavera, 1 cuando no haya ningún intermedio", "SECTORES"))
        sectores = 4
        Select Case sectores
            Case 1
                conparciales = False

            Case 2

            Case 3
                puntosector1 = 1254
                puntosector2 = 3083
                puntosector3 = 5000
                puntosector4 = 5000

            Case 4
                puntosector1 = 1254
                puntosector2 = 1254 + 1086
                puntosector3 = 1254 + 1086 + 1484
                puntosector4 = 5000

            Case Else
                conparciales = False

        End Select

        Call Button2_Click(sender, e)



        OpenFileDialog1.Filter = "Archivos de datos XML|*.xml"
        'OpenFileDialog1.FileName = "current.xml"
        OpenFileDialog1.ShowDialog()
        archivodatosxml = OpenFileDialog1.FileName
        'archivodatosxml = "b:\current.xml"
        For i As Integer = 0 To 1000
            enpista(i) = False
            indesadoElNumero(i) = False
        Next
        indice = 0
        Label1.BackColor = Color.Magenta
        Label1.Text = "         P 1          "
        Label2.BackColor = Color.GreenYellow
        Label2.Text = "MISMA VUELTA QUE P1   "
        Label3.BackColor = Color.Aqua
        Label3.Text = "1 VUELTA  MENOS QUE P1"
        Label4.BackColor = Color.CadetBlue
        Label4.Text = "2 VUELTAS MENOS QUE P1"



        Timer1.Enabled = True

        Timer1.Start()
        'Timer1.Stop()
        Timer1.Interval = 28


        Timer2.Enabled = True
        Timer2.Start()
        'Timer2.Stop()20
        Timer2.Interval = 350

        'Label1.Visible = False
        'Label2.Visible = False
        ' Label3.Visible = False
        ' Label4.Visible = False
        ' Label5.Visible = False
        Leer_Fichero.Visible = False
        Grabar_Coord.Visible = False
        comenzar.Visible = False
        Dibujar.Visible = False
        Button2.Visible = False



        porcentajesector1 = ((puntosector1) / puntosector4) + 0.00001
        porcentajesector2 = (((puntosector2 - puntosector1)) / puntosector4) + 0.00001
        porcentajesector3 = (((puntosector3 - puntosector2)) / puntosector4) + 0.00001
        porcentajesector4 = (((puntosector4 - puntosector3)) / puntosector4) + 0.00001


    End Sub

    Private Sub Leer_Fichero_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles Leer_Fichero.Click
        Dim textoArchivo, LineaTexto As String
        ' textoArchivo = "C:\Documents and Settings\Propietario\Mis documentos\Para Borrar\albacete_puntos.txt"
        OpenFileDialog1.Filter = "Archivos de texto (*.txt)|*.txt"
        OpenFileDialog1.ShowDialog()
        textoArchivo = OpenFileDialog1.FileName

        If textoArchivo <> "" Then
            Try
                'FileOpen(1, OpenFileDialog1.FileName, OpenMode.Input)
                FileOpen(1, textoArchivo, OpenMode.Input)
                Do Until EOF(1)
                    'LineaTexto = LineInput(1)
                    'textoArchivo = textoArchivo & LineaTexto & vbCrLf
                    divisiones = Val(LineInput(1))
                    xdelpunto(divisiones) = Val(LineInput(1) * 1.0025)
                    ydelpunto(divisiones) = Val(LineInput(1) * 1.3) + 40


                Loop
                'RichTextBox1.Text = textoArchivo
            Catch e1 As FileNotFoundException
                MessageBox.Show("Error al abrir el archivo:" & e1.Message)
            Finally
                FileClose(1)
            End Try
        End If
    End Sub



    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Dim LineaTexto As String
        Dim i As Integer = 0
        Dim Nombre_Archivo As String
        ' My.Computer.FileSystem.DeleteFile("G:\current.xml")  ' Borra el fichero current.xml


        'OpenFileDialog1.Filter = "Archivos de coordenadas (*.dxf)|*.dxf"
        'OpenFileDialog1.ShowDialog()
        'Nombre_Archivo = OpenFileDialog1.FileName
        Nombre_Archivo = "Navarra_Nuevo_Trazado_1366x768.txt"

        If Nombre_Archivo <> "" Then
            Try
                FileOpen(1, Nombre_Archivo, OpenMode.Input)
                Do Until EOF(1)
                    LineaTexto = LineInput(1)
                    If LineaTexto = "POINT" Then
                        'i = i + 1
                        For j As Integer = 1 To 11
                            LineaTexto = LineInput(1)
                        Next
                        divisiones = i
                        xdelpunto(divisiones) = Int(Val(LineInput(1)) * 0.99)
                        xdelpunto(divisiones) = Int((PictureBox1.Size.Width * xdelpunto(divisiones)) / 1366)
                        LineaTexto = LineInput(1)
                        ydelpunto(divisiones) = Int(Val(LineInput(1)) * 1)
                        ydelpunto(divisiones) = 768 - ydelpunto(divisiones)
                        ydelpunto(divisiones) = Int((PictureBox1.Size.Height * ydelpunto(divisiones)) / 768) - 25 '-15
                        LineaTexto = LineInput(1)
                        LineaTexto = LineInput(1)
                        LineaTexto = LineInput(1)
                        xdelpuntoInicial(divisiones) = xdelpunto(divisiones)
                        ydelpuntoInicial(divisiones) = ydelpunto(divisiones)
                        i = i + 1
                    End If

                Loop
                'RichTextBox1.Text = textoArchivo
            Catch e1 As FileNotFoundException
                MessageBox.Show("Error al abrir el archivo:" & e1.Message)
            Finally
                FileClose(1)
            End Try
        End If

        Label6.Location = New Point(PictureBox1.Width / 1.8, PictureBox1.Height / 1.5)
        Label7.Location = New Point(Label6.Location.X + 100, Label6.Location.Y - 20)
        If PictureBox1.Height < 500 Then
            Label6.Visible = False
            Label7.Visible = False
        Else
            Label6.Visible = True
            Label7.Visible = True
        End If


    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick


        '.Font = New Font("Arial", 11)
        'For i As Integer = 1 To indice
        For i As Integer = indice To 1 Step -1
            corredor_1 = Val(numeroDelIndice(i))
            If enpista(corredor_1) Then
                If Not asignadolabel(corredor_1) Then
                    lblmoto(corredor_1) = New Label
                    With lblmoto(corredor_1)
                        .Parent = PictureBox1
                        .AutoSize = True
                        .Visible = True
                        .BackColor = Color.GreenYellow
                        .Font = New Font("Arial Black", 14)
                        .BorderStyle = BorderStyle.FixedSingle
                        .BringToFront()  'trae el control al primer plano
                        '.Text = corredor_1
                        .Text = numeroDelIndice(i)
                        ' .Location = New Point(xdelpunto(corredor_1) - (lblmoto(corredor_1).Size.Width / 2), ydelpunto(1))
                    End With
                    asignadolabel(corredor_1) = True
                End If


                If tiempo_de_vuelta(corredor_1) = 0 Then
                    ' tiempo_de_vuelta(corredor_1) = TiempoultimavueltaTomada(corredor_1)
                    tiempo_de_vuelta(corredor_1) = 120
                End If

                If marker(corredor_1) = "6" Then
                    tiempo_de_vuelta(corredor_1) = Tiempodeultimavueltatratada(corredor_1) * 1.25
                End If

                tiempo_entre_marcas(corredor_1) = tiempo_de_vuelta(corredor_1) / divisiones
                division_asignada(corredor_1) = Int((DateAndTime.Timer - HoraDePasoPormeta(corredor_1)) / tiempo_entre_marcas(corredor_1))
                If division_asignada(corredor_1) > divisiones Then division_asignada(corredor_1) = divisiones

                If division_asignada(corredor_1) < 0 Then division_asignada(corredor_1) = 1


                ''''''''If conparciales = False Then             '''**********   Se asigna el tiempo entre divisiones y la division donde tiene que colocarse el corredor ****************

                ''''''''tiempo_entre_marcas(corredor_1) = tiempo_de_vuelta(corredor_1) / divisiones
                ''''''''division_asignada(corredor_1) = Int((DateAndTime.Timer - HoraDePasoPormeta(corredor_1)) / tiempo_entre_marcas(corredor_1))
                ''''''''If division_asignada(corredor_1) > divisiones Then division_asignada(corredor_1) = divisiones

                ''''''''If division_asignada(corredor_1) < 0 Then division_asignada(corredor_1) = 1


                '************** Para establecer el control en los puntos intermedios  ***********************
                '*****  SI LLEGA  MAS TARDE
                '  --> ESPERA.   en el punto de control(Intermedio) correspondiente y continua cuando llega el piloto
                '       --COMO ACTUAR: Cuando ultimatimeline= timeline anterior a la que estamos tratando.
                '                       igualamos la divisionasignada con el punto del sector --> division_asignada(corredor_1) = puntosector1
                '                       y no se actualiza el tiempo de vuelta estimado.  el tiempo de vuelta se calcula  cuando leega al  punto de control
                '               Cuando  ultimatimeline = Ultimo punto de control(el sector que estamos tratando)
                '            -- Si la division asignada es > que la que corresponde al punto de control. Entonces hacemos  divisionaasignada = division del punto de control
                '       CONSECUENCIAS: Cuando llega el piloto al punto de control hace un salto hasta la division que le hubiera correspondio por el tiempo que ha estado parado
                '       COMO CORREGIRLO:   RECALCULAMOS EL TIEMPO DE LA  VUELTA.
                '       COMO LO HACEMOS:   como conocemos el porcentaje que es cada parcial sobre el tiempo completo de la vuelta y el tiempo hasta el punto de control (hora de paso por el contro - hora de paso por meta)
                '                          incrementamos en ese mismo porcentaje el tiempo de tiempo_de_vuelta() + lo que se incrementa por el porcentaje.
                ' '*****  SI LLEGA  ANTES
                '            -->  Salta hasta el punto de contro y calcula el tiempo_de_vuelta()
                '
                '
                If conparciales = True Then        '    conparciales=false para cuando no hay parciales y no pasa por estos if     conparciales=true  para cuando hay parciales


                    If marker(corredor_1) <> 6 Then       '  Bandera a cuadros


                        If ultimatimeline(corredor_1) = "" Then
                            division_asignada(corredor_1) = 0
                            tiempo_de_vuelta(corredor_1) = 0

                        ElseIf ultimatimeline(corredor_1) = "Finish Lane" OrElse ultimatimeline(corredor_1) = "Pit-Out" OrElse ultimatimeline(corredor_1) = "V Max2" OrElse ultimatimeline(corredor_1) = "V Max1" Then


                            If division_asignada(corredor_1) > puntosector1 Then   'LLEGA MAS TARDE AL SECTOR 1  espera en el sector 1 a que  llegue el corredor, (el corredor llega mas tarde)

                                'Acctualiza el tiempo de vuelta                                  calcual el tiempo del sector

                                division_asignada(corredor_1) = puntosector1
                                ' tiempo_de_vuelta(corredor_1) = tiempo_de_vuelta(corredor_1) * ((DateAndTime.Timer - HoraDePasoPormeta(corredor_1)) / tiemposector1(corredor_1))
                                'HoraDePasoPorSector1(corredor_1) = DateAndTime.Timer
                                tiemposector1(corredor_1) = DateAndTime.Timer - HoraDePasoPormeta(corredor_1)
                                tiempo_de_vuelta(corredor_1) = tiemposector1(corredor_1) / porcentajesector1
                                If tiempo_de_vuelta(corredor_1) < 120 And ultimatimeline(corredor_1) = "Pit-Out" Then tiempo_de_vuelta(corredor_1) = 120

                                tiemposector1(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector1
                                tiemposector2(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector2
                                tiemposector3(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector3
                                tiemposector4(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector4

                            End If


                        ElseIf ultimatimeline(corredor_1) = "Intermediate 1" Then
                            If division_asignada(corredor_1) <= puntosector1 Then    ' LLEGA ANTES AL SECTOR 1  le ponemos en el punto del sector1
                                HoraDePasoPorSector1(corredor_1) = DateAndTime.Timer
                                ' tiempo_de_vuelta(corredor_1) = tiempo_de_vuelta(corredor_1) * ((DateAndTime.Timer - HoraDePasoPormeta(corredor_1)) / tiemposector1(corredor_1))
                                tiemposector1(corredor_1) = (HoraDePasoPorSector1(corredor_1) - HoraDePasoPormeta(corredor_1))               ' tiemposector1(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector1
                                tiempo_de_vuelta(corredor_1) = tiemposector1(corredor_1) / porcentajesector1

                                tiemposector1(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector1
                                tiemposector2(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector2
                                tiemposector3(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector3
                                tiemposector4(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector4

                            ElseIf division_asignada(corredor_1) > puntosector2 Then      'EL VEHICULO (no el label) LLEGA MAS TARDE AL SECTOR 2 espera en el sector 2 a que  llegue el corredor
                                HoraDePasoPorSector2(corredor_1) = DateAndTime.Timer
                                division_asignada(corredor_1) = puntosector2



                                tiemposector2(corredor_1) = DateAndTime.Timer - HoraDePasoPormeta(corredor_1)

                                tiempo_de_vuelta(corredor_1) = tiemposector2(corredor_1) / (porcentajesector2 + porcentajesector1)


                                tiemposector1(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector1
                                tiemposector2(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector2
                                tiemposector3(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector3
                                tiemposector4(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector4

                            End If


                        ElseIf ultimatimeline(corredor_1) = "Intermediate 2" Then
                            If division_asignada(corredor_1) <= puntosector2 Then    ' LLEGA ANTES AL SECTOR 2  le ponemos en el punto del sector1
                                HoraDePasoPorSector2(corredor_1) = DateAndTime.Timer

                                tiemposector2(corredor_1) = (HoraDePasoPorSector2(corredor_1) - HoraDePasoPormeta(corredor_1))               ' tiemposector2(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector2


                                tiempo_de_vuelta(corredor_1) = tiemposector2(corredor_1) / (porcentajesector2 + porcentajesector1)

                                ' division_asignada(corredor_1) = Int((DateAndTime.Timer - HoraDePasoPormeta(corredor_1)) / tiempo_entre_marcas(corredor_1))

                                'tiemposector1(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector1
                                'tiemposector2(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector2
                                'tiemposector3(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector3
                                'tiemposector4(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector4

                            ElseIf division_asignada(corredor_1) > puntosector3 Then      'EL VEHICULO (no el label) LLEGA MAS TARDE AL SECTOR 2 espera en el sector 2 a que  llegue el corredor
                                HoraDePasoPorSector3(corredor_1) = DateAndTime.Timer
                                division_asignada(corredor_1) = puntosector3

                                tiemposector3(corredor_1) = DateAndTime.Timer - HoraDePasoPormeta(corredor_1)


                                tiempo_de_vuelta(corredor_1) = tiemposector3(corredor_1) / (porcentajesector3 + porcentajesector2 + porcentajesector1)

                                'tiemposector1(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector1
                                'tiemposector2(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector2
                                ''tiemposector3(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector3
                                'tiemposector4(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector4

                            End If



                        ElseIf ultimatimeline(corredor_1) = "Intermediate 3" Then
                            If division_asignada(corredor_1) <= puntosector3 Then    ' LLEGA ANTES AL SECTOR 3  le ponemos en el punto del sector2

                                tiemposector3(corredor_1) = DateAndTime.Timer - HoraDePasoPormeta(corredor_1)    ' Nos referimos al tiempo del primer y segundo y tercer sector juntos y no solo al del segundo sector


                                tiempo_de_vuelta(corredor_1) = tiemposector3(corredor_1) / (porcentajesector3 + porcentajesector2 + porcentajesector1)   ' sumamos los dosporcentajes por referirnos a los dos secdtores juntos.


                                'tiemposector1(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector1
                                'tiemposector2(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector2
                                ''tiemposector3(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector3
                                'tiemposector4(corredor_1) = tiempo_de_vuelta(corredor_1) * porcentajesector4
                            End If

                        End If
                    End If
                End If


                'Label1.Text = division_asignada(corredor_1)
                'Label2.Text = tiempo_de_vuelta(corredor_1)
                'Label3.Text = tiemposector3(corredor_1)
                'Label4.Text = HoraDePasoPormeta(corredor_1)

                'Label1.Text = division_asignada(1)
                'Label2.Text = tiempo_de_vuelta(1)
                'Label3.Text = tiemposector3(1)
                'Label4.Text = ultimatimeline(1)


                If PictureBox1.Height < 500 Then
                    lblmoto(corredor_1).Font = New Font("Arial", 7)
                ElseIf PictureBox1.Height > 500 Then
                    lblmoto(corredor_1).Font = New Font("Arial Black", 14)
                End If



                '******************Para el cambio de colores según las vueltas y el primer puesto  ******************
                If posicion(corredor_1) = 1 Then
                    CorredorCabezadeCarrera = corredor_1
                    lblmoto(corredor_1).BackColor = Color.Magenta   ' cambia el color al cabeza de carrera
                    lblmoto(corredor_1).ForeColor = Color.Black
                    lblmoto(corredor_1).BringToFront()




                ElseIf vueltas(corredor_1) = vueltas(CorredorCabezadeCarrera) Then
                    'lblmoto(corredor_1).BackColor = Color.Yellow                       ' para cuando estan en la misma vuelta que el primero
                    lblmoto(corredor_1).BackColor = Color.GreenYellow
                    lblmoto(corredor_1).ForeColor = Color.Black
                ElseIf vueltas(corredor_1) = vueltas(CorredorCabezadeCarrera) - 1 Then   ' para cuando tienen una vuelta menos que el primero
                    lblmoto(corredor_1).BackColor = Color.Aqua
                    lblmoto(corredor_1).ForeColor = Color.Black
                Else
                    lblmoto(corredor_1).BackColor = Color.CadetBlue                     ' para cuando no están en la misma vuelta que el  primero
                    lblmoto(corredor_1).ForeColor = Color.Black
                End If

                '*********** Para cuando el corredor toma bandera a cuadros. *******************
                If marker(corredor_1) = "6" Then
                        lblmoto(corredor_1).BackColor = Color.Black
                    lblmoto(corredor_1).ForeColor = Color.White
                    If division_asignada(corredor_1) > 300 And enpista(corredor_1) Then   'Para cuando toma bandera  y se omite despues de 300 divisiones  para que no de toda la vuelta.
                        enpista(corredor_1) = False
                        asignadolabel(corredor_1) = False
                        lblmoto(corredor_1).Visible = False
                        lblmoto(corredor_1).Hide()
                    End If

                End If


                lblmoto(corredor_1).Location = New Point(xdelpunto(division_asignada(corredor_1)) - (lblmoto(corredor_1).Size.Width / 2), ydelpunto(division_asignada(corredor_1)))


                'If vueltas(CorredorCabezadeCarrera) > 0 Then
                '    Label6.Text = vueltas(CorredorCabezadeCarrera)
                'End If

                '***************
                'para cambiar color del fondo  en retirados 
                'para cuando no se completa la primera vuelta.
                If TiempoultimavueltaTomada(corredor_1) = 0 Then

                    If (DateAndTime.Timer - HoraDePasoPormeta(corredor_1)) > 150 And (DateAndTime.Timer - HoraDePasoPormeta(corredor_1)) < 170 Then
                        lblmoto(corredor_1).BackColor = Color.OrangeRed

                    ElseIf (DateAndTime.Timer - HoraDePasoPormeta(corredor_1)) > 170 Then
                        'asignadolabel(corredor_1) = False
                        'lblmoto(corredor_1).Hide()
                        'lblmoto(corredor_1).Visible = False
                        'enpista(corredor_1) = False
                    End If

                ElseIf TiempoultimavueltaTomada(corredor_1) > 0 Then  '

                    If (DateAndTime.Timer - (HoraDePasoPormeta(corredor_1) + Tiempodeultimavueltatratada(corredor_1))) > 15 Then 'And (DateAndTime.Timer - (HoraDePasoPormeta(corredor_1) + tiempo_de_vuelta(corredor_1))) < 30 Then
                        lblmoto(corredor_1).BackColor = Color.Red

                        ' ElseIf (DateAndTime.Timer - (HoraDePasoPormeta(corredor_1) + tiempo_de_vuelta(corredor_1))) > 10 Then
                        '     asignadolabel(corredor_1) = False
                        '   'lblmoto(corredor_1).Hide()
                        '   lblmoto(corredor_1).Visible = False
                        '  enpista(corredor_1) = False

                    End If
                End If



            End If
        Next

    End Sub


    Private Sub Timer2_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer2.Tick
        Dim k As Integer
        Dim p As Integer
        Dim p2 As Integer
        Dim p3 As Integer
        Dim minutos As Integer
        Dim segundos As Single
        Dim milesimas As Integer
        Dim numero As Integer



        Dim HoraActual As Double
        Dim Horaprevistadepasopormeta(1000) As Double

        TiempoYVueltasQueFaltan()

        k = 0


        Try
            Dim documenetoxml As XmlDocument
            Dim nodelist As XmlNodeList
            Dim nodo As XmlNode
            documenetoxml = New XmlDocument

            documenetoxml.Load(archivodatosxml)





            'reset list of numbers that appear in the xml before reading it
            For i As Integer = 0 To 1000
                indesadoElNumero(i) = False
            Next

            nodelist = documenetoxml.SelectNodes("/resultspage/results/result")
            For Each nodo In nodelist
                k = k + 1

                numeroDelIndice(k) = (nodo.Attributes.GetNamedItem("no").Value)
                corredor_2 = Val(numeroDelIndice(k))
                indesadoElNumero(corredor_2) = True
                vueltas(corredor_2) = Val(nodo.Attributes.GetNamedItem("laps").Value)
                posicion(corredor_2) = Val(nodo.Attributes.GetNamedItem("position").Value)
                marker(corredor_2) = nodo.Attributes.GetNamedItem("marker").Value
                ultimatimeline(corredor_2) = nodo.Attributes.GetNamedItem("lasttimeline").Value
                HoraDelDiaActual(corredor_2) = nodo.Attributes.GetNamedItem("lasttimeofday").Value
                'tiemposector1(corredor_2) = nodo.Attributes.GetNamedItem("section0").Value
                'tiemposector2(corredor_2) = nodo.Attributes.GetNamedItem("section1").Value
                ' tiemposector3(corredor_2) = nodo.Attributes.GetNamedItem("section2").Value
                Dim variableauxiliar As String = nodo.Attributes.GetNamedItem("lasttime").Value
                If variableauxiliar = "" Then variableauxiliar = "2:00.0"

                'If ultimatimeline(corredor_2) = "Finish Lane" Then ultimatimeline(corredor_2) = "Start/Finish"
                'If ultimatimeline(corredor_2) = "Pit-Out" Then ultimatimeline(corredor_2) = "Pit Lane Exit"
                If ultimatimeline(corredor_2) = "Pit-Finish-Lane" Then ultimatimeline(corredor_2) = "Pit-Out"          '"Finish Lane"        '"Pit-Out"


                'buscamos el tiempo de la ultima vuelta
                p = InStr(1, variableauxiliar, ":")   'busca los caracteres ":" que se separan los minutos de los segundos.
                p2 = InStr(1, variableauxiliar, ".")   'busca los caracteres "." que se separan los  segundos de la milesimas.
                minutos = Val(Mid(variableauxiliar, 1, p)) 'devuelve los minutos de la ultima vuelta.
                segundos = Val(Mid(variableauxiliar, p + 1, p2)) 'devuelve los segundos de la ultima vuelta.
                'milesimas = Val(Mid(ultimodato, p + 9, p + 11)) 'devuelve las milesimas  de la ultima vuelta.
                segundos = minutos * 60 + segundos '+ milesimas / 1000   '
                TiempoultimavueltaTomada(corredor_2) = segundos    'utilizamos la variable "corredor_2" como integer y no "(numeroDelIndice(k))" que string para poder mostrar el numero con algún carácter
                ' If TiempoultimavueltaTomada(corredor_2) = 0 Then TiempoultimavueltaTomada(corredor_2) = 120
                ' If TiempoultimavueltaTomada(corredor_2) = 0 Or TiempoultimavueltaTomada(corredor_2) > 150 Then TiempoultimavueltaTomada(corredor_2) = 120


                'If vueltas(corredor_2) = 0 Then
                ''TiempoultimavueltaTomada(corredor_2) = 120
                'Tiempodeultimavueltatratada(corredor_2) = TiempoultimavueltaTomada(corredor_2)
                'ultimavueltatratada(corredor_2) = vueltas(corredor_2)
                'enpista(corredor_2) = True
                'HoraDePasoPormeta(corredor_2) = DateAndTime.Timer
                'tiempo_de_vuelta(corredor_2) = 120


                If HoraDelDiaActual(corredor_2) <> HoraDelDiaAnterior(corredor_2) And ((ultimatimeline(corredor_2) = "Finish Lane" OrElse ultimatimeline(corredor_2) = "Pit-Out")) Then

                    HoraDelDiaAnterior(corredor_2) = HoraDelDiaActual(corredor_2)
                    enpista(corredor_2) = True
                    HoraDePasoPormeta(corredor_2) = DateAndTime.Timer

                    tiempo_de_vuelta(corredor_2) = TiempoultimavueltaTomada(corredor_2)
                    Tiempodeultimavueltatratada(corredor_2) = TiempoultimavueltaTomada(corredor_2)

                    'Reset backcolor when receiving a fresh update
                    If asignadolabel(corredor_2) Then
                        lblmoto(corredor_2).BackColor = Color.GreenYellow
                    End If

                    If tiempo_de_vuelta(corredor_2) = 0 Then tiempo_de_vuelta(corredor_2) = 120


                    'ElseIf ultimatimeline(corredor_2) = "Intermediate 1" Then    ' esto no está bien,  hay que revisar
                    '    If enpista(corredor_2) = False Then
                    '        enpista(corredor_2) = True
                    '        'HoraDePasoPormeta(corredor_2) = DateAndTime.Timer - 30
                    '        tiempo_de_vuelta(corredor_2) = 120

                    '        'division_asignada(corredor_2) = puntosector1

                    '        If division_asignada(corredor_2) <= puntosector1 Then    ' LLEGA ANTES AL SECTOR 1  le ponemos en el punto del sector1
                    '            HoraDePasoPorSector1(corredor_2) = DateAndTime.Timer
                    '            ' tiempo_de_vuelta(corredor_2) = tiempo_de_vuelta(corredor_2) * ((DateAndTime.Timer - HoraDePasoPormeta(corredor_2)) / tiemposector1(corredor_2))
                    '            tiemposector1(corredor_2) = (HoraDePasoPorSector1(corredor_2) - HoraDePasoPormeta(corredor_2))               ' tiemposector1(corredor_2) = tiempo_de_vuelta(corredor_2) * porcentajesector1
                    '            tiempo_de_vuelta(corredor_2) = tiemposector1(corredor_2) / porcentajesector1

                    '            tiemposector1(corredor_2) = tiempo_de_vuelta(corredor_2) * porcentajesector1
                    '            tiemposector2(corredor_2) = tiempo_de_vuelta(corredor_2) * porcentajesector2
                    '            tiemposector3(corredor_2) = tiempo_de_vuelta(corredor_2) * porcentajesector3
                    '            tiemposector4(corredor_2) = tiempo_de_vuelta(corredor_2) * porcentajesector4

                    '        ElseIf division_asignada(corredor_2) > puntosector2 Then      'EL VEHICULO (no el label) LLEGA MAS TARDE AL SECTOR 2 espera en el sector 2 a que  llegue el corredor
                    '            HoraDePasoPorSector2(corredor_2) = DateAndTime.Timer
                    '            division_asignada(corredor_2) = puntosector2



                    '            tiemposector2(corredor_2) = DateAndTime.Timer - HoraDePasoPormeta(corredor_2)

                    '            tiempo_de_vuelta(corredor_2) = tiemposector2(corredor_2) / (porcentajesector2 + porcentajesector1)


                    '            tiemposector1(corredor_2) = tiempo_de_vuelta(corredor_2) * porcentajesector1
                    '            tiemposector2(corredor_2) = tiempo_de_vuelta(corredor_2) * porcentajesector2
                    '            tiemposector3(corredor_2) = tiempo_de_vuelta(corredor_2) * porcentajesector3
                    '            tiemposector4(corredor_2) = tiempo_de_vuelta(corredor_2) * porcentajesector4

                    'End If
                    'ElseIf HoraDelDiaActual(corredor_2) <> HoraDelDiaAnterior(corredor_2) And (ultimatimeline(corredor_2) = "Intermediate 1") Then
                    '    'HoraDelDiaAnterior(corredor_2) = HoraDelDiaActual(corredor_2)
                    '    enpista(corredor_2) = True
                    '    'HoraDePasoPormeta(corredor_2) = DateAndTime.Timer

                    '    'tiempo_de_vuelta(corredor_2) = TiempoultimavueltaTomada(corredor_2)
                    '    'Tiempodeultimavueltatratada(corredor_2) = TiempoultimavueltaTomada(corredor_2)

                    '    If tiempo_de_vuelta(corredor_2) = 120 Then 'Then tiempo_de_vuelta(corredor_2) = 120

                    '    ElseIf HoraDelDiaActual(corredor_2) <> HoraDelDiaAnterior(corredor_2) And (ultimatimeline(corredor_2) = "Intermediate 2") Then
                    '        'HoraDelDiaAnterior(corredor_2) = HoraDelDiaActual(corredor_2)
                    '        enpista(corredor_2) = True
                    '        'HoraDePasoPormeta(corredor_2) = DateAndTime.Timer

                    '        'tiempo_de_vuelta(corredor_2) = TiempoultimavueltaTomada(corredor_2)
                    '        'Tiempodeultimavueltatratada(corredor_2) = TiempoultimavueltaTomada(corredor_2)

                    '        If tiempo_de_vuelta(corredor_2) = 0 Then tiempo_de_vuelta(corredor_2) = 120

                    '    ElseIf HoraDelDiaActual(corredor_2) <> HoraDelDiaAnterior(corredor_2) And (ultimatimeline(corredor_2) = "Intermediate 3") Then

                    'HoraDelDiaAnterior(corredor_2) = HoraDelDiaActual(corredor_2)
                    'enpista(corredor_2) = True
                    ' HoraDePasoPormeta(corredor_2) = DateAndTime.Timer

                    'tiempo_de_vuelta(corredor_2) = TiempoultimavueltaTomada(corredor_2)
                    'Tiempodeultimavueltatratada(corredor_2) = TiempoultimavueltaTomada(corredor_2)

                    If tiempo_de_vuelta(corredor_2) = 0 Then tiempo_de_vuelta(corredor_2) = 120

                End If
                'Else

                'End If



                HoraActual = DateAndTime.Timer

                If marker(corredor_2) = "7" Or marker((corredor_2)) = "8" Or marker((corredor_2)) = "9" Or marker((corredor_2)) = "10" Or marker((corredor_2)) = "11" Then  ' Pit in, No salido, Retirado, Excluido
                    enpista(corredor_2) = False
                    asignadolabel(corredor_2) = False
                    lblmoto(corredor_2).Visible = False
                    lblmoto(corredor_2).Hide()
                    'ultimavueltatratada(corredor_2) = 0
                ElseIf marker(corredor_2) = "12" Then   'Pit Exit
                    enpista(corredor_2) = True
                End If


                If (HoraActual - (HoraDePasoPormeta(corredor_2) + tiempo_de_vuelta(corredor_2))) > 30 And enpista(corredor_2) Then '  And OtravezEnPista(corredor_2) = False Then
                    enpista(corredor_2) = False
                    asignadolabel(corredor_2) = False
                    lblmoto(corredor_2).Visible = False
                    lblmoto(corredor_2).Hide()
                    tiempo_de_vuelta(corredor_2) = 0
                End If


                'MsgBox(idimagen)
            Next

            'hide any labels that didn't appear in the latest xml
            For i As Integer = 1 To 1000
                If asignadolabel(i) AndAlso indesadoElNumero(i) = False Then
                    lblmoto(i).Visible = False
                    lblmoto(i).Hide()
                    asignadolabel(i) = False
                    enpista(i) = False
                End If
            Next

            indice = k
            Exit Try

        Catch ex As Exception


            ' MsgBox(ex.ToString())

        End Try
        indice = k
    End Sub



    Private Sub PictureBox1_DoubleClick(sender As Object, e As EventArgs) Handles PictureBox1.DoubleClick
        For i As Integer = 1 To 1000
            vueltas(i) = 0
            Tiempodeultimavueltatratada(i) = 0
            TiempoultimavueltaTomada(i) = 0
            tiempo_de_vuelta(i) = 0
            ultimavueltatratada(i) = -1
            HoraDelDiaAnterior(i) = "xx"
            HoraDelDiaActual(i) = ""
            If asignadolabel(i) = True Then
                lblmoto(i).Visible = False
                lblmoto(i).Hide()
                enpista(i) = False
                asignadolabel(i) = False
            End If
        Next
        Label6.Text = ""

    End Sub

    Private Sub form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ''Dim monitor As Screen
        ''monitor = Screen.AllScreens(1)
        ''Me.Hide()
        ''Me.Show()

        For j As Integer = 0 To 1000
            HoraDelDiaAnterior(j) = "xx"
            HoraDelDiaActual(j) = ""
        Next
        Me.PictureBox1.Controls.Add(Me.Label6)   ' hace que el Label6 (cuenta Vueltas del primero) tenga el fondo transparente  coge el fondo del contenedor, el picturebox1
        Me.PictureBox1.Controls.Add(Me.Label7)
        Call comenzar_Click(sender, e)

        'Me.PictureBox1.Controls.Add(Me.Label6)   ' hace que el Label6 (cuenta Vueltas del primero) tenga el fondo transparente  coge el fondo del contenedor, el picturebox1
    End Sub


    Private Sub PictureBox1_SizeChanged(sender As Object, e As EventArgs) Handles PictureBox1.SizeChanged

        Call Button2_Click(sender, e)

    End Sub


    Private Sub TiempoYVueltasQueFaltan()
        Dim i As Integer
        Dim inicio As Integer
        Dim lineaarchivo As String
        Dim timetogo As String
        Dim lapstogo As String




        If archivodatosxml = "" Then Exit Sub
        If archivodatosxml <> "" Then
            Try
                FileOpen(1, archivodatosxml, OpenMode.Input)
                Do Until EOF(1)
                    i = i + 1
                    lineaarchivo = LineInput(1)

                    If i > 18 Then Exit Do
                Loop


                inicio = InStr(lineaarchivo, "timetogo") + 10
                timetogo = Mid(lineaarchivo, inicio, 20)
                timetogo = Mid(timetogo, 1, InStr(timetogo, "<") - 1)


                inicio = InStr(lineaarchivo, "lapstogo") + 10
                lapstogo = Mid(lineaarchivo, inicio, 5)
                lapstogo = Mid(lapstogo, 1, InStr(lapstogo, "<") - 1)



                If timetogo <> "" Then
                    Label7.Text = "Tiempo restante"
                    Label6.Text = timetogo
                ElseIf timetogo = "" Then
                    Label7.Text = "Vueltas restantes"
                    Label6.Text = lapstogo
                End If

            Catch ex As Exception


            End Try
        End If
        FileClose(1)

    End Sub


End Class


