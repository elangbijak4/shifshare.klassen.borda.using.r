mulai_aplikasi<-function() {
    require(gWidgetsRGtk2)
    require(xlsx)
    require(RWeka)
    
    #=======================================================================
    bukafile <- function(){
        dataku1<-data.frame()
        window = gwindow("File search", visible = TRUE)
        paned = gpanedgroup(cont = window)
        frmOutput = gframe("Output: ", cont=paned, horizontal = TRUE)
        size(frmOutput) = c(350, 200)
        #txtOutput = gtext("", cont = frmOutput, expand = TRUE)
        #size(txtOutput) = c(350, 200)
        
        # Create a button to open a .csv file
        btnImportFile = gfilebrowse(text = "Select a file ", quote = FALSE, type = "open", cont = frmOutput, filter = "*.xlsx")
        addHandlerChanged(btnImportFile, handler = function(h, ...){svalue(h$obj)})
        
        btnDescribeColumns = gbutton(text = "Buka/Import data", cont = frmOutput)
        dataku2<-addHandlerChanged(btnDescribeColumns, handler = function(h, ...){
            dataku1 = read.xlsx(svalue(btnImportFile), sheetIndex = 2)
            #dispose(
            n<-gtable(dataku1, multiple = TRUE, container = frmOutput)
            size(n)=c(340,420)
            print(dataku1)
            assign("dataku1", dataku1, envir = .GlobalEnv) 
        })
    }
    
    #=======================================================================
    #menulist
    mbl <- list()
    mbl$Dataset$Buka$handler = function(h,...){
        #wdata<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        #gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdata, toolkit = guiToolkit())
        
        #Create a button to open a .csv file
        btnImportFile = gfilebrowse(text = "Select a file ", quote = FALSE, type = "open", cont = frmOutput, filter = "*.xlsx")
        addHandlerChanged(btnImportFile, handler = function(h, ...){svalue(h$obj)})
        
        btnDescribeColumns = gbutton(text = "Buka/Import data", cont = frmOutput)
        dataku2<-addHandlerChanged(btnDescribeColumns, handler = function(h, ...){
            datakuxlsx = read.xlsx(svalue(btnImportFile), sheetIndex = 2)
            #dispose(
            n<-gtable(datakuxlsx, multiple = TRUE, container = frmOutput)
            size(n)=c(990,500)
            #print(dataku1)
            assign("dataku1", datakuxlsx, envir = .GlobalEnv) 
        })
        #bukafile()
    }
    mbl$Dataset$`Simpan Sebagai Excel`$handler = function(h,...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())
    }
    mbl$Dataset$`Simpan Sebagai CSV`$handler = function(h,...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())
    }
    mbl$Dataset$`Simpan Sebagai Xml`$handler = function(h,...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())
    }
    mbl$Dataset$`Simpan Sebagai Json`$handler = function(h,...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())
    }
    mbl$Dataset$Keluar$handler = function(h,...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())
    }
    mbl$Dataset$Keluar$icon = "quit"
    
    #Menu Hitungan Awal
    mbl$`Hitungan Awal`$`Tampilkan data semua sektor di tahun tertentu perkabupaten`$handler = function(h,...){
        wdt0<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Rata-rata pertumbuhan...", container = wdt0, toolkit = guiToolkit())
        paned3 = ggroup(horizontal=FALSE, cont = wdt0)
        frmOutput3 = gframe("Output: ", cont=paned3, horizontal = FALSE)
        size(frmOutput3) = c(999, 600)
        
        a<-ginput("Masukkan nilai tahun, default 2011", text="2011", icon="question")
        #a<-strsplit(a,":")
        #v<-as.vector(a[[1]][1]:a[[1]][2])
		a<-as.integer(a)
        m<-data_semua_sektor_tahun_tertentu(dataku1,a)
        #ratatumbuh<-cbind(kabupaten=dataku1[,1],m)
        k2<-gtable(m, multiple = TRUE, container = frmOutput3)
        size(k2)=c(990,500)
        #print(dataku1)
        assign("datasemuasektorpertahun", m, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma hitung rata-rata pertumbuhan sektor di konsol", container = frmOutput3, handler = function(h,...) {
            print(data_semua_sektor_tahun_tertentu)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput3, handler = function(h,...) {
            dispose(k2)
        })
    }
    
	mbl$`Hitungan Awal`$`Hitung data total sektor pertahun`$handler = function(h,...){
        wdt1<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        paned1 = ggroup(horizontal=FALSE, cont = wdt1)
        frmOutput1 = gframe("Output: ", cont=paned1, horizontal = FALSE)
        size(frmOutput1) = c(999, 600)
        
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt1, toolkit = guiToolkit())
        
        total<-totalPDRBpertahun(dataku1)
        total2<-cbind(kabupaten=dataku1[,1],total)
        k<-gtable(total2, multiple = TRUE, container = frmOutput1)
        size(k)=c(990,550)
        #print(dataku1)
        assign("datakutotal", total, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma hitung total sektor di konsol", container = frmOutput1, handler = function(h,...) {
            print(buatindex)
            print(totalPDRBpertahun)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput1, handler = function(h,...) {
            dispose(k)
            #size(k)=c(990,0)
        })
    }
    mbl$`Hitungan Awal`$`Hitung rata-rata pertumbuhan tiap sektor per kabupaten`$handler = function(h,...){
        wdt2<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Rata-rata pertumbuhan...", container = wdt2, toolkit = guiToolkit())
        paned2 = ggroup(horizontal=FALSE, cont = wdt2)
        frmOutput2 = gframe("Output: ", cont=paned2, horizontal = FALSE)
        size(frmOutput2) = c(999, 600)
        
        a<-ginput("Masukkan nilai rentang tahun, misal antara 2011 sampai 2015, tulis 2011:2015", text="2011:2012", icon="question")
        a<-strsplit(a,":")
        v<-as.vector(a[[1]][1]:a[[1]][2])
        m<-rataPertumbuhan_sesuai_disertasi(dataku1,v)
        #ratatumbuh<-cbind(kabupaten=dataku1[,1],m)
        k2<-gtable(m, multiple = TRUE, container = frmOutput2)
        size(k2)=c(990,500)
        #print(dataku1)
        assign("datakuratatumbuh", m, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma hitung rata-rata pertumbuhan sektor di konsol", container = frmOutput2, handler = function(h,...) {
            print(rataPertumbuhan_sesuai_disertasi)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput2, handler = function(h,...) {
            dispose(k2)
        })
    }
    mbl$`Hitungan Awal`$`Hitung rata-rata kontribusi tiap sektor per kabupaten`$handler = function(h,...){
        wdt3<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Rata-rata pertumbuhan...", container = wdt3, toolkit = guiToolkit())
        paned3 = ggroup(horizontal=FALSE, cont = wdt3)
        frmOutput3 = gframe("Output: ", cont=paned3, horizontal = FALSE)
        size(frmOutput3) = c(999, 600)
        
        a<-ginput("Masukkan nilai rentang tahun, misal antara 2011 sampai 2015, tulis 2011:2015", text="2011:2012", icon="question")
        a<-strsplit(a,":")
        v<-as.vector(a[[1]][1]:a[[1]][2])
        m<-rataKontribusi_sesuai_disertasi(dataku1,v)
        #ratatumbuh<-cbind(kabupaten=dataku1[,1],m)
        k2<-gtable(m, multiple = TRUE, container = frmOutput3)
        size(k2)=c(990,500)
        #print(dataku1)
        assign("datakontibusisektor", m, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma hitung rata-rata pertumbuhan sektor di konsol", container = frmOutput3, handler = function(h,...) {
            print(rataKontribusi_sesuai_disertasi)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput3, handler = function(h,...) {
            dispose(k2)
        })
    }
    mbl$`Hitungan Awal`$`Hitung pendapatan perkapita perkabupaten`$handler = function(h,...){
        wdt4<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Rata-rata pertumbuhan...", container = wdt4, toolkit = guiToolkit())
        paned3 = ggroup(horizontal=FALSE, cont = wdt4)
        frmOutput3 = gframe("Output: ", cont=paned3, horizontal = FALSE)
        size(frmOutput3) = c(999, 600)
        
        a<-ginput("Masukkan nilai rentang tahun, misal antara 2011 sampai 2015, tulis 2011:2015", text="2011:2012", icon="question")
        a<-strsplit(a,":")
        v<-as.vector(a[[1]][1]:a[[1]][2])
        m<-rataPertumbuhanPendapatanPerkapita(dataku1,,,v)
        #ratatumbuh<-cbind(kabupaten=dataku1[,1],m)
        k2<-gtable(m, multiple = TRUE, container = frmOutput3)
        size(k2)=c(990,500)
        #print(dataku1)
        assign("datapendapatanperkapita", m, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma hitung pendapatan perkapita di konsol", container = frmOutput3, handler = function(h,...) {
            print(rataPertumbuhanPendapatanPerkapita)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput3, handler = function(h,...) {
            dispose(k2)
        })
	}	
	
	mbl$`Hitungan Awal`$`Hitung rata-rata pertumbuhan kabupaten (semua sektor)`$handler = function(h,...){
        wdt41<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Rata-rata pertumbuhan semua sektor...", container = wdt41, toolkit = guiToolkit())
        paned3 = ggroup(horizontal=FALSE, cont = wdt41)
        frmOutput3 = gframe("Output: ", cont=paned3, horizontal = FALSE)
        size(frmOutput3) = c(999, 600)
        
        a<-ginput("Masukkan nilai rentang tahun, misal antara 2011 sampai 2015, tulis 2011:2015", text="2011:2012", icon="question")
        a<-strsplit(a,":")
        v<-as.vector(a[[1]][1]:a[[1]][2])
        m<-rataPertumbuhanSemuaSektor(dataku1,v)
        #ratatumbuh<-cbind(kabupaten=dataku1[,1],m)
        k2<-gtable(m, multiple = TRUE, container = frmOutput3)
        size(k2)=c(990,500)
        #print(dataku1)
        assign("datapertumbuhanperkabupaten", m, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma hitung rerata pertumbuhan kabupaten di konsol", container = frmOutput3, handler = function(h,...) {
            print(rataPertumbuhanSemuaSektor)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput3, handler = function(h,...) {
            dispose(k2)
        })
    }
    
    mbl$`Hitungan Awal`$`Hitung rata-rata kontribusi kabupaten (semua sektor)`$handler = function(h,...){
        wdt42<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Rata-rata pertumbuhan semua sektor...", container = wdt42, toolkit = guiToolkit())
        paned3 = ggroup(horizontal=FALSE, cont = wdt42)
        frmOutput3 = gframe("Output: ", cont=paned3, horizontal = FALSE)
        size(frmOutput3) = c(999, 600)
        
        a<-ginput("Masukkan nilai rentang tahun, misal antara 2011 sampai 2015, tulis 2011:2015", text="2011:2012", icon="question")
        a<-strsplit(a,":")
        v<-as.vector(a[[1]][1]:a[[1]][2])
        m<-rataKontribusiSemuaSektor(dataku1,v)
        #ratatumbuh<-cbind(kabupaten=dataku1[,1],m)
        k2<-gtable(m, multiple = TRUE, container = frmOutput3)
        size(k2)=c(990,500)
        #print(dataku1)
        assign("datakontribusiperkabupaten", m, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma hitung rerata kontribusi kabupaten di konsol", container = frmOutput3, handler = function(h,...) {
            print(rataKontribusiSemuaSektor)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput3, handler = function(h,...) {
            dispose(k2)
        })
    }
	
	#Menu Analisis Klassen
    mbl$`Analisis Klassen`$`Analisis Klassen per Sektor per Kabupaten`$handler = function(h,...){
        wdt5<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Rata-rata pertumbuhan...", container = wdt5, toolkit = guiToolkit())
        paned3 = ggroup(horizontal=FALSE, cont = wdt5)
        frmOutput3 = gframe("Output: ", cont=paned3, horizontal = FALSE)
        size(frmOutput3) = c(999, 600)
        
        a<-ginput("Masukkan nilai rentang tahun, misal antara 2011 sampai 2015, tulis 2011:2015", text="2011:2012", icon="question")
        a<-strsplit(a,":")
        v<-as.vector(a[[1]][1]:a[[1]][2])
        m<-analisisKlassen_PerSektor_PerKabupaten_Sesuai_disertasi(dataku1,,,v)
        #(dataframe=dataPDRB,indexWilayah=1:29, Acuan=30,range_tahun=2011:2015,range_sektor=1:9,vektor_index=c())
        #ratatumbuh<-cbind(kabupaten=dataku1[,1],m)
        k2<-gtable(m, multiple = TRUE, container = frmOutput3)
        size(k2)=c(990,500)
        #print(dataku1)
        assign("dataanalisisklassenpersektor", m, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma analisis klassen per sektor di konsol", container = frmOutput3, handler = function(h,...) {
            print(analisisKlassen_PerSektor_PerKabupaten_Sesuai_disertasi)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput3, handler = function(h,...) {
            dispose(k2)
        })
    }
    mbl$`Analisis Klassen`$`Analisis Klassen per Kabupaten`$handler = function(h,...){
        wdt6<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Rata-rata pertumbuhan...", container = wdt6, toolkit = guiToolkit())
        paned3 = ggroup(horizontal=FALSE, cont = wdt6)
        frmOutput3 = gframe("Output: ", cont=paned3, horizontal = FALSE)
        size(frmOutput3) = c(999, 600)
        
        a<-ginput("Masukkan nilai rentang tahun, misal antara 2011 sampai 2015, tulis 2011:2015", text="2011:2012", icon="question")
        a<-strsplit(a,":")
        v<-as.vector(a[[1]][1]:a[[1]][2])
        m<-analisisKlassen_Seluruh_Sektor(dataku1,,,v)
        #(dataframe=dataPDRB,indexWilayah=1:29, Acuan=30,range_tahun=2011:2015,range_sektor=1:9,vektor_index=c())
        #ratatumbuh<-cbind(kabupaten=dataku1[,1],m)
        k2<-gtable(m, multiple = TRUE, container = frmOutput3)
        size(k2)=c(990,500)
        #print(dataku1)
        assign("dataanalisisklassenperkabupaten", m, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma analisis klassen per kabupaten di konsol", container = frmOutput3, handler = function(h,...) {
            print(analisisKlassen_Seluruh_Sektor)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput3, handler = function(h,...) {
            dispose(k2)
        })
    }
	
	#Menu Analisis ShifShare
    mbl$`Analisis ShiftShare`$`Analisis ShiftShare (Differential Shift) per Sektor per Kabupaten`$handler = function(h,...){
        wdt7<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Differential Shift per sektor per kabupaten...", container = wdt7, toolkit = guiToolkit())
        paned3 = ggroup(horizontal=FALSE, cont = wdt7)
        frmOutput3 = gframe("Output: ", cont=paned3, horizontal = FALSE)
        size(frmOutput3) = c(999, 600)
        
        a<-ginput("Masukkan nilai rentang tahun, misal antara 2011 sampai 2015, tulis 2011:2015", text="2011:2012", icon="question")
        a<-strsplit(a,":")
        v<-as.vector(a[[1]][1]:a[[1]][2])
        m<-analisisShiftsharetotal_DS(dataku1,v)
        #(dataframe=dataPDRB,indexWilayah=1:29, Acuan=30,range_tahun=2011:2015,range_sektor=1:9,vektor_index=c())
        #ratatumbuh<-cbind(kabupaten=dataku1[,1],m)
        k2<-gtable(m, multiple = TRUE, container = frmOutput3)
        size(k2)=c(990,500)
        #print(dataku1)
        assign("dataanalisisShiftsharetotal_DS", m, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma analisis shiftshare (Differential Shift) per sektor per kabupaten di konsol", container = frmOutput3, handler = function(h,...) {
            print(analisisShiftshare)
			print(analisisShiftsharetotal_DS)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput3, handler = function(h,...) {
            dispose(k2)
        })
    }
    mbl$`Analisis ShiftShare`$`Analisis ShiftShare (Proportional Shift) per Sektor per Kabupaten`$handler = function(h,...){
        wdt8<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Differential Shift per sektor per kabupaten...", container = wdt8, toolkit = guiToolkit())
        paned3 = ggroup(horizontal=FALSE, cont = wdt8)
        frmOutput3 = gframe("Output: ", cont=paned3, horizontal = FALSE)
        size(frmOutput3) = c(999, 600)
        
        a<-ginput("Masukkan nilai rentang tahun, misal antara 2011 sampai 2015, tulis 2011:2015", text="2011:2012", icon="question")
        a<-strsplit(a,":")
        v<-as.vector(a[[1]][1]:a[[1]][2])
        m<-analisisShiftsharetotal_P(dataku1,v)
        #(dataframe=dataPDRB,indexWilayah=1:29, Acuan=30,range_tahun=2011:2015,range_sektor=1:9,vektor_index=c())
        #ratatumbuh<-cbind(kabupaten=dataku1[,1],m)
        k2<-gtable(m, multiple = TRUE, container = frmOutput3)
        size(k2)=c(990,500)
        #print(dataku1)
        assign("dataanalisisShiftsharetotal_P", m, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma analisis shiftshare (Proportional Shift) per sektor per kabupaten di konsol", container = frmOutput3, handler = function(h,...) {
            print(analisisShiftshare)
			print(analisisShiftsharetotal_P)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput3, handler = function(h,...) {
            dispose(k2)
        })
    }
    
	mbl$`Analisis ShiftShare`$`Analisis ShiftShare (Provincial Share) per Sektor per Kabupaten`$handler = function(h,...){
        wdt8<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Differential Shift per sektor per kabupaten...", container = wdt8, toolkit = guiToolkit())
        paned3 = ggroup(horizontal=FALSE, cont = wdt8)
        frmOutput3 = gframe("Output: ", cont=paned3, horizontal = FALSE)
        size(frmOutput3) = c(999, 600)
        
        a<-ginput("Masukkan nilai rentang tahun, misal antara 2011 sampai 2015, tulis 2011:2015", text="2011:2012", icon="question")
        a<-strsplit(a,":")
        v<-as.vector(a[[1]][1]:a[[1]][2])
        m<-analisisShiftsharetotal_PS(dataku1,v)
        #(dataframe=dataPDRB,indexWilayah=1:29, Acuan=30,range_tahun=2011:2015,range_sektor=1:9,vektor_index=c())
        #ratatumbuh<-cbind(kabupaten=dataku1[,1],m)
        k2<-gtable(m, multiple = TRUE, container = frmOutput3)
        size(k2)=c(990,500)
        #print(dataku1)
        assign("dataanalisisShiftsharetotal_PS", m, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma analisis shiftshare (Provincial Share) per sektor per kabupaten di konsol", container = frmOutput3, handler = function(h,...) {
            print(analisisShiftshare)
			print(analisisShiftsharetotal_PS)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput3, handler = function(h,...) {
            dispose(k2)
        })
    }
	
	mbl$`Analisis ShiftShare`$`Analisis ShiftShare (Pertambahan Nilai=PShare+ProShift+DiffShift)/Sektor/Kabupaten`$handler = function(h,...){
        wdt8<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Differential Shift per sektor per kabupaten...", container = wdt8, toolkit = guiToolkit())
        paned3 = ggroup(horizontal=FALSE, cont = wdt8)
        frmOutput3 = gframe("Output: ", cont=paned3, horizontal = FALSE)
        size(frmOutput3) = c(999, 600)
        
        a<-ginput("Masukkan nilai rentang tahun, misal antara 2011 sampai 2015, tulis 2011:2015", text="2011:2012", icon="question")
        a<-strsplit(a,":")
        v<-as.vector(a[[1]][1]:a[[1]][2])
        m<-analisisShiftsharetotal_Delta(dataku1,v)
        #(dataframe=dataPDRB,indexWilayah=1:29, Acuan=30,range_tahun=2011:2015,range_sektor=1:9,vektor_index=c())
        #ratatumbuh<-cbind(kabupaten=dataku1[,1],m)
        k2<-gtable(m, multiple = TRUE, container = frmOutput3)
        size(k2)=c(990,500)
        #print(dataku1)
        assign("dataanalisisShiftsharetotal_Delta", m, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma analisis shiftshare (Pertambahan Nilai) per sektor per kabupaten di konsol", container = frmOutput3, handler = function(h,...) {
            print(analisisShiftshare)
			print(analisisShiftsharetotal_Delta)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput3, handler = function(h,...) {
            dispose(k2)
        })
    }

	mbl$`Analisis ShiftShare`$`Analisis ShiftShare Differential Shift per Kabupaten`$handler = function(h,...){
        wdt8<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Differential Shift per sektor per kabupaten...", container = wdt8, toolkit = guiToolkit())
        paned3 = ggroup(horizontal=FALSE, cont = wdt8)
        frmOutput3 = gframe("Output: ", cont=paned3, horizontal = FALSE)
        size(frmOutput3) = c(999, 600)
        
        a<-ginput("Masukkan nilai rentang tahun, misal antara 2011 sampai 2015, tulis 2011:2015", text="2011:2012", icon="question")
        a<-strsplit(a,":")
        v<-as.vector(a[[1]][1]:a[[1]][2])
        m<-analisisShiftsharetotal_DS_kabupaten(dataku1,v)
        #(dataframe=dataPDRB,indexWilayah=1:29, Acuan=30,range_tahun=2011:2015,range_sektor=1:9,vektor_index=c())
        #ratatumbuh<-cbind(kabupaten=dataku1[,1],m)
        k2<-gtable(m, multiple = TRUE, container = frmOutput3)
        size(k2)=c(990,500)
        #print(dataku1)
        assign("dataanalisisShiftsharetotal_DS_kabupaten", m, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma Analisis ShiftShare Differential Shift per Kabupaten di konsol", container = frmOutput3, handler = function(h,...) {
            print(analisisShiftshare_Kabupaten)
			print(analisisShiftsharetotal_DS_kabupaten)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput3, handler = function(h,...) {
            dispose(k2)
        })
    }	

	mbl$`Analisis ShiftShare`$`Analisis ShiftShare Provincial Share per Kabupaten`$handler = function(h,...){
        wdt8<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Differential Shift per sektor per kabupaten...", container = wdt8, toolkit = guiToolkit())
        paned3 = ggroup(horizontal=FALSE, cont = wdt8)
        frmOutput3 = gframe("Output: ", cont=paned3, horizontal = FALSE)
        size(frmOutput3) = c(999, 600)
        
        a<-ginput("Masukkan nilai rentang tahun, misal antara 2011 sampai 2015, tulis 2011:2015", text="2011:2012", icon="question")
        a<-strsplit(a,":")
        v<-as.vector(a[[1]][1]:a[[1]][2])
        m<-analisisShiftsharetotal_PS_kabupaten(dataku1,v)
        #(dataframe=dataPDRB,indexWilayah=1:29, Acuan=30,range_tahun=2011:2015,range_sektor=1:9,vektor_index=c())
        #ratatumbuh<-cbind(kabupaten=dataku1[,1],m)
        k2<-gtable(m, multiple = TRUE, container = frmOutput3)
        size(k2)=c(990,500)
        #print(dataku1)
        assign("dataanalisisShiftsharetotal_PS_kabupaten", m, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma Analisis ShiftShare Provincial Share per Kabupaten di konsol", container = frmOutput3, handler = function(h,...) {
            print(analisisShiftshare_Kabupaten)
			print(analisisShiftsharetotal_PS_kabupaten)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput3, handler = function(h,...) {
            dispose(k2)
        })
    }	

	mbl$`Analisis ShiftShare`$`Analisis ShiftShare Proportional Shift per Kabupaten`$handler = function(h,...){
        wdt8<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Differential Shift per sektor per kabupaten...", container = wdt8, toolkit = guiToolkit())
        paned3 = ggroup(horizontal=FALSE, cont = wdt8)
        frmOutput3 = gframe("Output: ", cont=paned3, horizontal = FALSE)
        size(frmOutput3) = c(999, 600)
        
        a<-ginput("Masukkan nilai rentang tahun, misal antara 2011 sampai 2015, tulis 2011:2015", text="2011:2012", icon="question")
        a<-strsplit(a,":")
        v<-as.vector(a[[1]][1]:a[[1]][2])
        m<-analisisShiftsharetotal_P_kabupaten(dataku1,v)
        #(dataframe=dataPDRB,indexWilayah=1:29, Acuan=30,range_tahun=2011:2015,range_sektor=1:9,vektor_index=c())
        #ratatumbuh<-cbind(kabupaten=dataku1[,1],m)
        k2<-gtable(m, multiple = TRUE, container = frmOutput3)
        size(k2)=c(990,500)
        #print(dataku1)
        assign("dataanalisisShiftsharetotal_P_kabupaten", m, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma Analisis ShiftShare Proportional Shift per Kabupaten di konsol", container = frmOutput3, handler = function(h,...) {
            print(analisisShiftshare_Kabupaten)
			print(analisisShiftsharetotal_P_kabupaten)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput3, handler = function(h,...) {
            dispose(k2)
        })
    }	

	mbl$`Analisis ShiftShare`$`Analisis ShiftShare Pertambahan Nilai (PShare+ProShift+DiffShift) per Kabupaten`$handler = function(h,...){
        wdt8<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Differential Shift per sektor per kabupaten...", container = wdt8, toolkit = guiToolkit())
        paned3 = ggroup(horizontal=FALSE, cont = wdt8)
        frmOutput3 = gframe("Output: ", cont=paned3, horizontal = FALSE)
        size(frmOutput3) = c(999, 600)
        
        a<-ginput("Masukkan nilai rentang tahun, misal antara 2011 sampai 2015, tulis 2011:2015", text="2011:2012", icon="question")
        a<-strsplit(a,":")
        v<-as.vector(a[[1]][1]:a[[1]][2])
        m<-analisisShiftsharetotal_Delta_kabupaten(dataku1,v)
        #(dataframe=dataPDRB,indexWilayah=1:29, Acuan=30,range_tahun=2011:2015,range_sektor=1:9,vektor_index=c())
        #ratatumbuh<-cbind(kabupaten=dataku1[,1],m)
        k2<-gtable(m, multiple = TRUE, container = frmOutput3)
        size(k2)=c(990,500)
        #print(dataku1)
        assign("dataanalisisShiftsharetotal_Delta_kabupaten", m, envir = .GlobalEnv) 
        
        gbutton("Tampilkan algoritma Analisis ShiftShare Pertambahan Nilai per Kabupaten di konsol", container = frmOutput3, handler = function(h,...) {
            print(analisisShiftshare_Kabupaten)
			print(analisisShiftsharetotal_Delta_kabupaten)
            gmessage("Algoritma ditampilkan di Konsol", icon = "info")
        })
        
        gbutton("Tutup", container = frmOutput3, handler = function(h,...) {
            dispose(k2)
        })
    }	

    #Menu View Algoritma
    mbl$`View Algoritma`$`Algoritma Klassen per Sektor per Kabupaten`$handler = function(h,...) print(analisisKlassen_PerSektor_PerKabupaten())
    mbl$`View Algoritma`$`Algoritma Klassen per Kabupaten`$handler = function(h,...) print(analisisKlassen_Seluruh_Sektor)
    mbl$`View Algoritma`$`Algoritma ShifShare per Sektor per Kabupaten`$handler = function(h,...) print(analisisShiftshare)
    mbl$`View Algoritma`$`Algoritma ShifShare per Kabupaten`$handler = function(h,...) print(analisisShiftshare_Kabupaten)
    
    #Menu Decision Tree
    mbl$`Decision Tree`$`J48 (C4.5)`$handler = function(h,...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())
    }
    mbl$`Decision Tree`$`NBTree (Naive Bayes Tree)`$handler = function(h,...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())
    }
    mbl$`Decision Tree`$`Random Forest`$handler = function(h,...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())
    }
    mbl$`Decision Tree`$`Random Tree`$handler = function(h,...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())
    }
    
    #Menu Analisis Borda
    mbl$`Analisis BORDA`$`Borda per Sektor per Kabupaten`$handler = function(h,...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())
    }
    mbl$`Analisis BORDA`$`Borda per Kabupaten`$handler = function(h,...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())
    }
    mbl$`Analisis BORDA`$`Borda versus Decision Tree`$handler = function(h,...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())
    }
    
    #Pending
    #tbl <- list()
    #tbl$Open <- list(label="Open", icon="new",handler = function(...) print("print"))
    #tbl$Save <- list(icon="save",handler = function(...) print("print"))
    #tbl$Load <- list(icon="connect",handler = function(...) print("print"))
    
    tblist <- list(open=gaction("Open", icon="open", handler=function(...){
        #wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        #gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())
        
        #Create a button to open a .csv file
        btnImportFile = gfilebrowse(text = "Select a file ", quote = FALSE, type = "open", cont = frmOutput, filter = "*.xlsx")
        addHandlerChanged(btnImportFile, handler = function(h, ...){svalue(h$obj)})
        
        btnDescribeColumns = gbutton(text = "Buka/Import data", cont = frmOutput)
        dataku2<-addHandlerChanged(btnDescribeColumns, handler = function(h, ...){
            datakuxlsx = read.xlsx(svalue(btnImportFile), sheetIndex = 2)
            #dispose(
            n<-gtable(datakuxlsx, multiple = TRUE, container = frmOutput)
            size(n)=c(990,500)
            #print(dataku1)
            assign("dataku1", datakuxlsx, envir = .GlobalEnv) 
        })
        
    }),
    
    separator=gseparator(),
    
    view=gaction("View Data", icon="matrix", handler=function(...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())}),
    
    separator=gseparator(),
    
    save=gaction("Save", icon="save", handler=function(...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())}),
    
    separator=gseparator(),
    
    klassen=gaction("Klassen", icon="newplot", handler=function(...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())}),
    
    separator=gseparator(),
    
    shiftshare=gaction("Shift Share", icon="boxplot", handler=function(...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())}),
    
    separator=gseparator(),
    
    borda=gaction("Borda", icon="dataframe", handler=function(...){
        wdt<-gwindow(title = "Analisis dan Prediksi Decision Tree", name = "heru_klassen2", width=700, height = 350)
        gstatusbar(text = "Silahkan pilih dataset terlebih dulu...", container = wdt, toolkit = guiToolkit())}),
    
    separator=gseparator(),
    
    keluar=gaction("Keluar", icon="quit", action=aQuit, handler=function(h,...){dispose(window)})
    )
    
    #=======================================================================
    #Buat window
    window<-gwindow(title = "Aplikasi Analisis Sektor Ungulan dan Kawasan Andalan Provinsi Papua oleh Heru", name = "heru_klassen", width=1000, height = 800)
    
    #Buat menu
    mb<-gmenu(mbl, action=NULL, container = window, toolkit = guiToolkit())
    
    #Buat toolbar
    tb <- gtoolbar(tblist, container=window)
    
    
    paned = ggroup(horizontal=FALSE, cont = window)
    frmOutput = gframe("Output: ", cont=paned, horizontal = FALSE)
    size(frmOutput) = c(999, 600)
    
    #Buat status bar
    gstatusbar(text = "Antarmuka utama aplikasi, silahkan pilih menu atau tombol yang tersedia", container = window, toolkit = guiToolkit())
}