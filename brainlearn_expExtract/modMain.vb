Module modMain

    Sub Main()
        Dim fichierPGN As String, fichierINI As String, pgnExtract As String, fichierUCI As String, moteurBIN As String, moteur_court As String, priorite As Integer, sourceBIN As String, cibleBIN As String
        Dim chaine As String, tabChaine() As String, tabTmp() As String, ligne As String, tabLigne() As String, i As Integer, j As Integer, positionDepart As String
        Dim lectureUCI As System.IO.TextReader, tailleFichier As Long, cumul As Long, nbParties As Integer, totCoups As Integer, depart As Integer
        Dim tabKEY(255) As String, chaineUCI As String, pos_reprise As Long, compteur_reprise As Integer, fichierREPRISE As String, totProf As Long
        Dim lectureBIN As IO.FileStream, tabTampon() As Byte, pas As Integer, tabEcriture() As Byte, compteur As Integer, nbPas As Integer, offsetProf As Integer
        Dim keyBIN As String, keyUCI As String

        fichierPGN = Replace(Command(), """", "")
        If fichierPGN = "" Then
            End
        End If
        If InStr(fichierPGN, "\") = 0 Then
            fichierPGN = My.Application.Info.DirectoryPath & "\" & fichierPGN
            If Not My.Computer.FileSystem.FileExists(fichierPGN) Then
                End
            End If
        End If

        cibleBIN = Replace(fichierPGN, ".pgn", "_experience.bin")

        'chargement parametres
        moteurBIN = "BrainLearn.exe"
        sourceBIN = "experience.bin"
        pgnExtract = "pgn-extract.exe"
        priorite = 64
        fichierINI = My.Computer.Name & ".ini"
        If My.Computer.FileSystem.FileExists(fichierINI) Then
            chaine = My.Computer.FileSystem.ReadAllText(fichierINI)
            If chaine <> "" And InStr(chaine, vbCrLf) > 0 Then
                tabChaine = Split(chaine, vbCrLf)
                For i = 0 To UBound(tabChaine)
                    If tabChaine(i) <> "" And InStr(tabChaine(i), " = ") > 0 Then
                        tabTmp = Split(tabChaine(i), " = ")
                        If tabTmp(0) <> "" And tabTmp(1) <> "" Then
                            If InStr(tabTmp(1), "//") > 0 Then
                                tabTmp(1) = Trim(gauche(tabTmp(1), tabTmp(1).IndexOf("//") - 1))
                            End If
                            Select Case tabTmp(0)
                                Case "moteurBIN"
                                    moteurBIN = tabTmp(1)
                                Case "sourceBIN"
                                    sourceBIN = tabTmp(1)
                                Case "pgnextract"
                                    pgnExtract = tabTmp(1)
                                Case "priorite"
                                    priorite = CInt(tabTmp(1))
                                Case Else

                            End Select
                        End If
                    End If
                Next
            End If
        End If
        My.Computer.FileSystem.WriteAllText(fichierINI, "moteurBIN = " & moteurBIN & vbCrLf _
                                                      & "sourceBIN = " & sourceBIN & vbCrLf _
                                                      & "pgnextract = " & pgnExtract & vbCrLf _
                                                      & "priorite = " & priorite & " //64 (idle), 16384 (below normal), 32 (normal), 32768 (above normal), 128 (high), 256 (realtime)" & vbCrLf, False)

        pos_reprise = 0
        compteur_reprise = 0
        fichierREPRISE = My.Computer.Name & "_reprise.ini"
        If My.Computer.FileSystem.FileExists(fichierREPRISE) Then
            chaine = My.Computer.FileSystem.ReadAllText(fichierREPRISE)
            If chaine <> "" And InStr(chaine, vbCrLf) > 0 Then
                tabChaine = Split(chaine, vbCrLf)
                For i = 0 To UBound(tabChaine)
                    If tabChaine(i) <> "" And InStr(tabChaine(i), " = ") > 0 Then
                        tabTmp = Split(tabChaine(i), " = ")
                        If tabTmp(0) <> "" And tabTmp(1) <> "" Then
                            If InStr(tabTmp(1), "//") > 0 Then
                                tabTmp(1) = Trim(gauche(tabTmp(1), tabTmp(1).IndexOf("//") - 1))
                            End If
                            Select Case tabTmp(0)
                                Case "index"
                                    pos_reprise = CLng(tabTmp(1))
                                Case "compteur"
                                    compteur_reprise = CInt(tabTmp(1))
                                Case Else

                            End Select
                        End If
                    End If
                Next
            End If
        End If

        '1°) convertir pgn en uci
        tailleFichier = 0
        fichierUCI = Replace(fichierPGN, ".pgn", "_uci.pgn")
        If My.Computer.FileSystem.FileExists(fichierUCI) Then
            My.Computer.FileSystem.DeleteFile(fichierUCI)
        End If
        pgnUCI(pgnExtract, fichierPGN, "_uci")
        Try
            tailleFichier = FileLen(fichierUCI)
        Catch ex As Exception
            End
        End Try

        '2°) convertir uci en clé zobrist
        lectureUCI = My.Computer.FileSystem.OpenTextFileReader(fichierUCI)
        moteur_court = Replace(nomFichier(moteurBIN), ".exe", "")
        chargerMoteur(moteurBIN)
        tabKEY(Convert.ToInt16("FB", 16)) = "FB592F56D4018F8F;"
        nbParties = 0
        totCoups = 0
        positionDepart = ""
        depart = Environment.TickCount
        Do
            ligne = lectureUCI.ReadLine()
            cumul = cumul + Len(ligne) + 2 'vbcrlf

            If ligne <> "" And InStr(ligne, "[") = 0 And InStr(ligne, "]") = 0 And InStr(ligne, """") = 0 Then
                i = 0
                nbParties = nbParties + 1
                chaineUCI = ""
                tabLigne = Split(ligne, " ")
                Do
                    chaineUCI = chaineUCI & tabLigne(i) & " "
                    If positionDepart = "" Then
                        keyUCI = uciKEY(entree, sortie, Trim(chaineUCI))
                    Else
                        keyUCI = uciKEY(entree, sortie, Trim(chaineUCI), positionDepart)
                    End If

                    keyBIN = ""
                    For j = 0 To 15 Step 2
                        keyBIN = keyUCI.Substring(j, 2) & keyBIN
                    Next
                    keyUCI = keyBIN & ";"
                    keyBIN = ""

                    If InStr(tabKEY(Convert.ToInt16(gauche(keyUCI, 2), 16)), keyUCI) = 0 Then
                        tabKEY(Convert.ToInt16(gauche(keyUCI, 2), 16)) = tabKEY(Convert.ToInt16(gauche(keyUCI, 2), 16)) & keyUCI
                        totCoups = totCoups + 1

                        If nbParties Mod 500 = 0 Then
                            Console.Clear()
                            Console.Title = My.Computer.Name & " : conversion @ " & Format(cumul / tailleFichier, "0%") & " (" & Trim(Format(1000 * totCoups / (Environment.TickCount - depart), "# ##0")) & " moves/sec, " & heureFin(depart, cumul, tailleFichier, , True) & ")"
                            Console.WriteLine("games   : " & Trim(Format(nbParties, "# ### ### ##0")))
                            Console.WriteLine("moves   : " & Trim(Format(totCoups, "# ### ### ##0")))
                            Console.WriteLine("average : " & Trim(Format(totCoups / nbParties, "# ### ### ##0") & " moves/game"))
                        End If
                    End If

                    i = i + 1
                Loop Until InStr(tabLigne(i), "/") > 0 Or InStr(tabLigne(i), "-") > 0 Or InStr(tabLigne(i), "*") > 0
                positionDepart = ""
            ElseIf ligne <> "" Then

                If InStr(ligne, "[FEN ", CompareMethod.Text) > 0 Then
                    positionDepart = Replace(Replace(ligne, "[FEN """, "", , , CompareMethod.Text), """]", "")
                End If

            End If
        Loop Until ligne Is Nothing

        Console.Clear()
        Console.Title = My.Computer.Name & " : conversion @ " & Format(cumul / tailleFichier, "0%") & " (" & Trim(Format(1000 * totCoups / (Environment.TickCount - depart), "# ##0")) & " moves/sec)"
        Console.WriteLine("games   : " & Trim(Format(nbParties, "# ### ### ##0")))
        Console.WriteLine("moves   : " & Trim(Format(totCoups, "# ### ### ##0")))
        Console.WriteLine("average : " & Trim(Format(totCoups / nbParties, "# ### ### ##0") & " moves/game"))
        lectureUCI.Close()
        dechargerMoteur()

        If My.Computer.FileSystem.FileExists(fichierUCI) Then
            My.Computer.FileSystem.DeleteFile(fichierUCI)
        End If

        '3°) récupérer les données d'experience
        lectureBIN = New IO.FileStream(sourceBIN, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)
        
        pas = 24
        offsetProf = 8

        nbPas = 1000000

        ReDim tabTampon(nbPas * pas - 1)
        ReDim tabEcriture(pas - 1)
        compteur = 0

        If pos_reprise > 0 Then
            lectureBIN.Position = pos_reprise
            compteur = compteur_reprise
        End If
        depart = Environment.TickCount
        totProf = 0
        Do
            lectureBIN.Read(tabTampon, 0, nbPas * pas)
            For j = 0 To nbPas - 1
                keyBIN = ""
                For k = 0 To 7
                    keyBIN = keyBIN & hexa(tabTampon(j * pas + k))
                Next
                keyBIN = keyBIN & ";"

                If InStr(tabKEY(Convert.ToInt16(gauche(keyBIN, 2), 16)), keyBIN) > 0 Then
                    Array.Copy(tabTampon, j * pas, tabEcriture, 0, pas)
                    My.Computer.FileSystem.WriteAllBytes(cibleBIN, tabEcriture, True)
                    compteur = compteur + 1
                    totProf = totProf + tabEcriture(offsetProf)

                    If compteur_reprise < compteur And compteur Mod 50 = 0 Then
                        Console.Clear()
                        Console.Title = My.Computer.Name & " : extraction @ " & Format(lectureBIN.Position / lectureBIN.Length, "0.00%") & " (" & Trim(Format(1000 * (compteur - compteur_reprise) / (Environment.TickCount - depart), "# ##0")) & " entries/sec, " & heureFin(depart, lectureBIN.Position, lectureBIN.Length, pos_reprise, True) & ")"
                        Console.WriteLine("entries   : " & Trim(Format(compteur, "# ### ### ##0")))
                        Console.WriteLine("positions : " & Trim(Format(totCoups, "# ### ### ##0")))
                        Console.WriteLine("avg depth : " & Trim(Format(totProf / (compteur - compteur_reprise), "##0")))
                    End If
                End If
            Next

            Console.Clear()
            If compteur_reprise < compteur Then
                Console.Title = My.Computer.Name & " : extraction @ " & Format(lectureBIN.Position / lectureBIN.Length, "0.00%") & " (" & Trim(Format(1000 * (compteur - compteur_reprise) / (Environment.TickCount - depart), "# ##0")) & " entries/sec, " & heureFin(depart, lectureBIN.Position, lectureBIN.Length, pos_reprise, True) & ")"
                Console.WriteLine("entries   : " & Trim(Format(compteur, "# ### ### ##0")))
                Console.WriteLine("positions : " & Trim(Format(totCoups, "# ### ### ##0")))
                Console.WriteLine("avg depth : " & Trim(Format(totProf / (compteur - compteur_reprise), "##0")))
            Else
                Console.Title = My.Computer.Name & " : extraction @ " & Format(lectureBIN.Position / lectureBIN.Length, "0.00%") & " (" & heureFin(depart, lectureBIN.Position, lectureBIN.Length, pos_reprise, True) & ")"
                Console.WriteLine("entries   : " & Trim(Format(compteur, "# ### ### ##0")))
                Console.WriteLine("positions : " & Trim(Format(totCoups, "# ### ### ##0")))
                Console.WriteLine("avg depth : " & Trim(Format(totProf, "##0")))
            End If

            My.Computer.FileSystem.WriteAllText(fichierREPRISE, "index = " & lectureBIN.Position & vbCrLf & "compteur = " & compteur & vbCrLf, False)

        Loop While lectureBIN.Position < lectureBIN.Length

        Console.Clear()
        Console.Title = My.Computer.Name & " : extraction @ " & Format(lectureBIN.Position / lectureBIN.Length, "0.00%") & " (" & Trim(Format(1000 * (compteur - compteur_reprise) / (Environment.TickCount - depart), "# ##0")) & " entries/sec)"
        Console.WriteLine("entries   : " & Trim(Format(compteur, "# ### ### ##0")))
        Console.WriteLine("positions : " & Trim(Format(totCoups, "# ### ### ##0")))
        Console.WriteLine("avg depth : " & Trim(Format(totProf / (compteur - compteur_reprise), "##0")))

        lectureBIN.Close()

        If My.Computer.FileSystem.FileExists(fichierREPRISE) Then
            My.Computer.FileSystem.DeleteFile(fichierREPRISE)
        End If

        Console.WriteLine("Press ENTER to close this window.")
        Console.ReadLine()
    End Sub

End Module
