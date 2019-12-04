#EKSEKUSI SEMUA ALGORITMA INI KETIKA APLIKASI DIJALANKAN DI AWAL. ATAU BUAT DUA FUNGSI:
#FUNGSI INISIALISASI (BUAT PENANDA VARIABEL KALAU SUDAH DIINISIALSIASI, MISAL INISIALSIASI=TRUE)
#FUNGSI MULAI_APLIKASI()
#.
buatindex <- function(v=list(
				index_semua_sektor2011=c(2,7,12,17,22,27,32,37,42),
                index_semua_sektor2012=c(3,8,13,18,23,28,33,38,43),
                index_semua_sektor2013=c(4,9,14,19,24,29,34,39,44),
                index_semua_sektor2014=c(5,10,15,20,25,30,35,40,45),
                index_semua_sektor2015=c(6,11,16,21,26,31,36,41,46))){
    unlist(v)
    #print(v[1])
    index<-data.frame(v[1])
    for(i in 2:length(v)){
        index=cbind(index,v[i])
    }
    buatindex<-index
}

#==================================================================================================================================
totalPDRBpertahun <- function(dataframe,vektor_index=list()){
    banyak_rekord_total<-nrow(dataframe)
    banyak_rekord_kab<-banyak_rekord_total-2
    banyak_rekord_kabprov<-banyak_rekord_total-1
    if(length(vektor_index)==0){index_semua_sektor<-buatindex()}else{index_semua_sektor<-buatindex(vektor_index)}
    m<-data.frame()
    for(name_index in names(index_semua_sektor) ){
        v<-unlist(strsplit(name_index,"[_]"))
        p<-paste(v[2],v[3],sep="_")
        for(i in 1:banyak_rekord_total){
            m[i,p]=0
            for(j in index_semua_sektor[,name_index]){
                m[i,p]=sum(m[i,p],dataframe[i,j])
            }
        }
    }
    totalPDRBpertahun<-m
}

#==================================================================================================================================
rataPertumbuhan <-
function(dataframe,range_tahun, vektor_index=c()){
    #pdrb<-totalPDRBpertahun(dataframe)
    if(length(vektor_index)==0){index_semua_sektor<-buatindex()}else{index_semua_sektor<-buatindex(vektor_index)}
    names(index_semua_sektor)=c(2011,2012,2013,2014,2015)
    rerata<-data.frame()
    #Hitung rata pertumbuhan tiap kabupaten, provinsi dan nasional
    for(i in 1:nrow(dataframe)){
        #print(i)
        for(j in 1:nrow(index_semua_sektor)){
            P=dataframe[i,index_semua_sektor[j,toString(range_tahun[length(range_tahun)])]]
            Q=dataframe[i,index_semua_sektor[j,toString(range_tahun[1])]]
            if((P==0)&&(Q==0)){rerata[i,j]=0}else if(Q==0){
                rerata[i,j]=(P/4)*100 
            } else{
                rerata[i,j]=(((P-Q)/(Q))/(length(range_tahun)-1))*100
            }
            
        }
    }
    rerata<-cbind(dataframe[,1],rerata)
    names(rerata)=c("nama.wilayah","rataTumbuh_S1","rataTumbuh_S2","rataTumbuh_S3","rataTumbuh_S4","rataTumbuh_S5","rataTumbuh_S6","rataTumbuh_S7","rataTumbuh_S8","rataTumbuh_S9")
    rataPertumbuhan<-rerata
}

#==================================================================================================================================
rataPertumbuhan_sesuai_disertasi <-
    function(dataframe=dataPDRB,range_tahun=2011:2012, vektor_index=list()){
        #pdrb<-totalPDRBpertahun(dataframe)
        if(length(vektor_index)==0){index_semua_sektor<-buatindex()}else{index_semua_sektor<-buatindex(vektor_index)}
        names(index_semua_sektor)=c(2011,2012,2013,2014,2015)
        rerata<-data.frame()
        #Hitung rata pertumbuhan tiap kabupaten, provinsi dan nasional
        for(i in 1:nrow(dataframe)){
            #print(i)
            for(j in 1:nrow(index_semua_sektor)){
                P=dataframe[i,index_semua_sektor[j,toString(range_tahun[length(range_tahun)])]]
                Q=dataframe[i,index_semua_sektor[j,toString(range_tahun[1])]]
                if((P==0)&&(Q==0)){rerata[i,j]=0}else if(Q==0){
                    rerata[i,j]=(P/4)
                } else{
                    rerata[i,j]=(((P-Q)/(Q))/(length(range_tahun)-1))
                }
                
            }
        }
        rerata<-cbind(dataframe[,1],rerata)
        names(rerata)=c("nama.wilayah","rataTumbuh_S1","rataTumbuh_S2","rataTumbuh_S3","rataTumbuh_S4","rataTumbuh_S5","rataTumbuh_S6","rataTumbuh_S7","rataTumbuh_S8","rataTumbuh_S9")
        rataPertumbuhan_sesuai_disertasi <-rerata
    }
#==================================================================================================================================
rataKontribusi <- function(dataframe,range_tahun,vektor_index=c()){
    totalpdrb<-totalPDRBpertahun(dataframe)
    if(length(vektor_index)==0){index_semua_sektor<-buatindex()}else{index_semua_sektor<-buatindex(vektor_index)}
    names(index_semua_sektor)=c(2011,2012,2013,2014,2015)
    names(totalpdrb)=c(2011,2012,2013,2014,2015)
    kontribusi<-data.frame()
    #Hitung kontribusi tiap kabupaten, provinsi dan nasional
    for(i in 1:nrow(dataframe)){
        for(j in 1:nrow(index_semua_sektor)){
            P=dataframe[i,index_semua_sektor[j,toString(range_tahun[length(range_tahun)])]]
            Q=dataframe[i,index_semua_sektor[j,toString(range_tahun[1])]]
            kontribusi[i,j]=((P+Q)/(totalpdrb[i,toString(range_tahun[1])]+totalpdrb[i,toString(range_tahun[length(range_tahun)])]))*100
        }
    }
    kontribusi<-cbind(dataframe[,1],kontribusi)
    names(kontribusi)<-c("nama.wilayah","kontribusi_S1","kontribusi_S2","kontribusi_S3","kontribusi_S4","kontribusi_S5","kontribusi_S6","kontribusi_S7","kontribusi_S8","kontribusi_S9")
    rataKontribusi<-kontribusi
}
#==================================================================================================================================
rataKontribusi_sesuai_disertasi <- function(dataframe=dataPDRB,range_tahun=2011:2012,vektor_index=c()){
    totalpdrb<-totalPDRBpertahun(dataframe)
    if(length(vektor_index)==0){index_semua_sektor<-buatindex()}else{index_semua_sektor<-buatindex(vektor_index)}
    names(index_semua_sektor)=c(2011,2012,2013,2014,2015)
    names(totalpdrb)=c(2011,2012,2013,2014,2015)
    kontribusi<-data.frame()
    #Hitung kontribusi tiap kabupaten, provinsi dan nasional
    for(i in 1:nrow(dataframe)){
        for(j in 1:nrow(index_semua_sektor)){
            P=dataframe[i,index_semua_sektor[j,toString(range_tahun[length(range_tahun)])]]
            Q=dataframe[i,index_semua_sektor[j,toString(range_tahun[1])]]
            kontribusi[i,j]=((P+Q)/(totalpdrb[i,toString(range_tahun[1])]+totalpdrb[i,toString(range_tahun[length(range_tahun)])]))
        }
    }
    kontribusi<-cbind(dataframe[,1],kontribusi)
    names(kontribusi)<-c("nama.wilayah","kontribusi_S1","kontribusi_S2","kontribusi_S3","kontribusi_S4","kontribusi_S5","kontribusi_S6","kontribusi_S7","kontribusi_S8","kontribusi_S9")
    rataKontribusi_sesuai_disertasi<-kontribusi
}
#==================================================================================================================================
analisisKlassen_PerSektor_PerKabupaten <-
function(dataframe=dataPDRB,indexWilayah=1:29, Acuan=30,range_tahun=2011:2015,range_sektor=1:9,vektor_index=c()) {
    pertumbuhan<-rataPertumbuhan(dataframe,range_tahun)
    kontribusi<-rataKontribusi(dataframe,range_tahun)
    kelas<-data.frame()
    
    #Analisis Klassen tiap kabupaten, tabel dataPDRB dikurangi rekord provinsi dan nasional (dikurangi k=2), tetapi k bisa berubah menuruti susunan tabel 
    for(i in indexWilayah){
        #r adalah jumlah sektor yang dianalisis, Acuan adalah nilai baris/index dari wilayah pada tabel dataPDRB yang dijadikan acuan, dapat dirubah.
        for(j in range_sektor){
            if((pertumbuhan[i,j+1]>=pertumbuhan[Acuan,j+1])&&(kontribusi[i,j+1]>=kontribusi[Acuan,j+1])){kelas[i,j]=1}
            else if((pertumbuhan[i,j+1]<pertumbuhan[Acuan,j+1])&&(kontribusi[i,j+1]>=kontribusi[Acuan,j+1])){kelas[i,j]=2}
            else if((pertumbuhan[i,j+1]>=pertumbuhan[Acuan,j+1])&&(kontribusi[i,j+1]<kontribusi[Acuan,j+1])){kelas[i,j]=3}
            else {kelas[i,j]=4}
        }
    }
    
    #Hitung sektor andalan tiap kabupaten
    sektorAndalan<-data.frame()
    for(i in 1:nrow(kelas)){
        m<-c()
        for(j in 1:ncol(kelas)){
            if((kelas[i,j]==1)||(kelas[i,j]==2)){m=append(m,paste("S",toString(j),sep=""))}
            m<-paste(m,sep=" ")
        }
        sektorAndalan[i,1]=toString(m)
    }
    #print(sektorAndalan)
    
    #Hitung keandalan sebuah wilayah, berdasarkan rata-rata nilai kuadran setiap sektor yang dibulatkan
    #rata_Kuadran_Kawasan<-data.frame()
    #for(i in 1:nrow(kelas)){
    #    h<-0
    #    for(j in 1:ncol(kelas)){
    #        if((kelas[i,j]==1)||(kelas[i,j]==2)){h=h+1}
    #        #h=h+kelas[i,j]
    #    }
    #    rata_Kuadran_Kawasan[i,"rata_Kuadran_Kawasan"]=h
    #}
    
    kelas<-cbind(dataframe[indexWilayah,1],kelas,sektorAndalan)
    names(kelas)=c("nama.wilayah","Kuadran_S1","Kuadran_S2","Kuadran_S3","Kuadran_S4","Kuadran_S5","Kuadran_S6","Kuadran_S7","Kuadran_S8","Kuadran_S9","Sektor_Andalan")
    analisisKlassen_PerSektor_PerKabupaten<-kelas
}
#==================================================================================================================================
analisisKlassen_PerSektor_PerKabupaten_Sesuai_disertasi <-
function(dataframe=dataPDRB,indexWilayah=1:29, Acuan=30,range_tahun=2011:2015,range_sektor=1:9,vektor_index=c()) {
    pertumbuhan<-rataPertumbuhan_sesuai_disertasi(dataframe,range_tahun)
    kontribusi<-rataKontribusi_sesuai_disertasi(dataframe,range_tahun)
    kelas<-data.frame()
    
    #Analisis Klassen tiap kabupaten, tabel dataPDRB dikurangi rekord provinsi dan nasional (dikurangi k=2), tetapi k bisa berubah menuruti susunan tabel 
    for(i in indexWilayah){
        #r adalah jumlah sektor yang dianalisis, Acuan adalah nilai baris/index dari wilayah pada tabel dataPDRB yang dijadikan acuan, dapat dirubah.
        for(j in range_sektor){
            if((pertumbuhan[i,j+1]>=pertumbuhan[Acuan,j+1])&&(kontribusi[i,j+1]>=kontribusi[Acuan,j+1])){kelas[i,j]=1}
            else if((pertumbuhan[i,j+1]<pertumbuhan[Acuan,j+1])&&(kontribusi[i,j+1]>=kontribusi[Acuan,j+1])){kelas[i,j]=2}
            else if((pertumbuhan[i,j+1]>=pertumbuhan[Acuan,j+1])&&(kontribusi[i,j+1]<kontribusi[Acuan,j+1])){kelas[i,j]=3}
            else {kelas[i,j]=4}
        }
    }
    
    #Hitung sektor andalan tiap kabupaten
    sektorAndalan<-data.frame()
    for(i in 1:nrow(kelas)){
        m<-c()
        for(j in 1:ncol(kelas)){
            if((kelas[i,j]==1)||(kelas[i,j]==2)){m=append(m,paste("S",toString(j),sep=""))}
            m<-paste(m,sep=" ")
        }
        sektorAndalan[i,1]=toString(m)
    }
    #print(sektorAndalan)
    
    #Hitung keandalan sebuah wilayah, berdasarkan rata-rata nilai kuadran setiap sektor yang dibulatkan
    #rata_Kuadran_Kawasan<-data.frame()
    #for(i in 1:nrow(kelas)){
    #    h<-0
    #    for(j in 1:ncol(kelas)){
    #        if((kelas[i,j]==1)||(kelas[i,j]==2)){h=h+1}
    #        #h=h+kelas[i,j]
    #    }
    #    rata_Kuadran_Kawasan[i,"rata_Kuadran_Kawasan"]=h
    #}
    
    kelas<-cbind(dataframe[indexWilayah,1],kelas,sektorAndalan)
    names(kelas)=c("nama.wilayah","Kuadran_S1","Kuadran_S2","Kuadran_S3","Kuadran_S4","Kuadran_S5","Kuadran_S6","Kuadran_S7","Kuadran_S8","Kuadran_S9","Sektor_Andalan")
    analisisKlassen_PerSektor_PerKabupaten<-kelas
}
#==================================================================================================================================
rataPertumbuhanPendapatanPerkapita <- function(dataframe=dataPDRB, indexWilayah=1:29, Acuan=30, range_tahun=2011:2015, vektor_index=c()) {
    pdrb <- totalPDRBpertahun(dataframe,vektor_index)
    rata<-data.frame()
    
    #Hitung rata-rata perkapita
    for(i in 1:(length(indexWilayah)+1)) {
        rata[i,"rata-rata_pendapatan_perkapita"]=0
        for(j in 1:length(range_tahun)) {
            rata[i,"rata-rata_pendapatan_perkapita"]<-(rata[i,"rata-rata_pendapatan_perkapita"]+pdrb[i,j])/length(range_tahun)
        }
        if(i==Acuan){rata[i,"rata-rata_pendapatan_perkapita"]=rata[i,"rata-rata_pendapatan_perkapita"]/length(indexWilayah)}
    }
    
    rata<-cbind(dataframe[1:(length(indexWilayah)+1),1],rata)
    names(rata)=c("nama.wilayah","rata-rata_pendapatan_perkapita")
    rataPertumbuhanPendapatanPerkapita <- rata
}
#==================================================================================================================================
rataPertumbuhanSemuaSektor <- function(dataframe=dataPDRB,range_tahun=2011:2015, vektor_index=c()) {
    pdrb<-totalPDRBpertahun(dataframe, vektor_index)
    #Ubah nama kolom ke tahun
    m<-c()
    for (i in 1:ncol(pdrb)){
        m<-append(m,substr(names(pdrb[i]),13,16))
    }
    names(pdrb)=m
    #print(pdrb)
    
    rerata<-data.frame()
    #Hitung rata pertumbuhan tiap kabupaten, provinsi dan nasional, berdasarkan seluruh sektor
    for(i in 1:nrow(dataframe)){
        for(j in 1){
            rerata[i,j]=(((pdrb[i,toString(range_tahun[length(range_tahun)])]-pdrb[i,toString(range_tahun[1])])/pdrb[i,toString(range_tahun[1])])/(length(range_tahun)-1))*100
        }
    }
    rerata<-cbind(dataframe[,1],rerata)
    names(rerata)=c("nama.wilayah","nilai_pertumbuhan_dari_seluruh_sektor")
    rataPertumbuhanSemuaSektor<-rerata
}

#UPDATE:
rataPertumbuhanSemuaSektor <- function(dataframe=dataPDRB,range_tahun=2011:2015, vektor_index=list()) {
    pdrb<-totalPDRBpertahun(dataframe, vektor_index)
    #Ubah nama kolom ke tahun
    m<-c()
    for (i in 1:ncol(pdrb)){
        m<-append(m,substr(names(pdrb[i]),13,16))
    }
    names(pdrb)=m
    #print(pdrb)
    
    rerata<-data.frame()
    #Hitung rata pertumbuhan tiap kabupaten, provinsi dan nasional, berdasarkan seluruh sektor
    for(i in 1:nrow(dataframe)){
        for(j in 1){
            rerata[i,j]=(((pdrb[i,toString(range_tahun[length(range_tahun)])]-pdrb[i,toString(range_tahun[1])])/pdrb[i,toString(range_tahun[1])])/(length(range_tahun)-1))*100
        }
    }
    rerata<-cbind(dataframe[,1],rerata)
    names(rerata)=c("nama.wilayah","nilai_pertumbuhan_dari_seluruh_sektor")
    rataPertumbuhanSemuaSektor<-rerata
}

#UPDATE: UNTUK DISERTASI
rataPertumbuhanSemuaSektor <- function(dataframe=dataPDRB,range_tahun=2011:2015, vektor_index=list()) {
    pdrb<-totalPDRBpertahun(dataframe, vektor_index)
    #Ubah nama kolom ke tahun
    m<-c()
    for (i in 1:ncol(pdrb)){
        m<-append(m,substr(names(pdrb[i]),13,16))
    }
    names(pdrb)=m
    #print(pdrb)
    
    rerata<-data.frame()
    #Hitung rata pertumbuhan tiap kabupaten, provinsi dan nasional, berdasarkan seluruh sektor
    for(i in 1:nrow(dataframe)){
        for(j in 1){
            rerata[i,j]=(((pdrb[i,toString(range_tahun[length(range_tahun)])]-pdrb[i,toString(range_tahun[1])])/pdrb[i,toString(range_tahun[1])])/(length(range_tahun)-1))
        }
    }
    rerata<-cbind(dataframe[,1],rerata)
    names(rerata)=c("nama.wilayah","nilai_pertumbuhan_dari_seluruh_sektor")
    rataPertumbuhanSemuaSektor<-rerata
}

#==================================================================================================================================
rataKontribusiSemuaSektor <- function(dataframe=dataPDRB,range_tahun=2011:2015, vektor_index=c()) {
    totalpdrb<-totalPDRBpertahun(dataframe, vektor_index)
    #Ubah nama kolom ke tahun
    m<-c()
    for (i in 1:ncol(totalpdrb)){
        m<-append(m,substr(names(totalpdrb[i]),13,16))
    }
    names(totalpdrb)=m
    
    #Hitung total PDRB dari seluruh totalPDRB setiap seluruh kabupaten dan provinsi
    total<-data.frame()
    for(j in 1:ncol(totalpdrb)){
        total[1,j]=0
        for(i in 1:(nrow(totalpdrb)-2)){
            total[1,j]=total[1,j]+totalpdrb[i,j]
        }
    }
    names(total)=m
    #print(total)
    
    #Hitung kontribusi total setiap kabupaten dan provinsi
    kontribusi<-data.frame()
    for(i in 1:(nrow(totalpdrb)-1)){
        kontribusi[i,1]=(totalpdrb[i,toString(range_tahun[length(range_tahun)])]+totalpdrb[i,toString(range_tahun[1])])/(total[1,toString(range_tahun[length(range_tahun)])]+total[1,toString(range_tahun[1])])
    }
    kontribusi[nrow(totalpdrb)-1,1]=kontribusi[nrow(totalpdrb)-1,1]/(nrow(totalpdrb)-2)
    kontribusi<-cbind(dataframe[1:30,1],kontribusi)
    names(kontribusi)=c("nama.wilayah","nilai_kontribusi_dari_seluruh_sektor")
    rataKontribusiSemuaSektor<-kontribusi
}

#UPDATE:
rataKontribusiSemuaSektor <- function(dataframe=dataPDRB,range_tahun=2011:2015, vektor_index=list()) {
    totalpdrb<-totalPDRBpertahun(dataframe, vektor_index)
    #Ubah nama kolom ke tahun
    m<-c()
    for (i in 1:ncol(totalpdrb)){
        m<-append(m,substr(names(totalpdrb[i]),13,16))
    }
    names(totalpdrb)=m
    
    #Hitung total PDRB dari seluruh totalPDRB setiap seluruh kabupaten dan provinsi
    total<-data.frame()
    for(j in 1:ncol(totalpdrb)){
        total[1,j]=0
        for(i in 1:(nrow(totalpdrb)-2)){
            total[1,j]=total[1,j]+totalpdrb[i,j]
        }
    }
    names(total)=m
    #print(total)
    
    #Hitung kontribusi total setiap kabupaten dan provinsi
    kontribusi<-data.frame()
    for(i in 1:(nrow(totalpdrb)-1)){
        kontribusi[i,1]=(totalpdrb[i,toString(range_tahun[length(range_tahun)])]+totalpdrb[i,toString(range_tahun[1])])/(total[1,toString(range_tahun[length(range_tahun)])]+total[1,toString(range_tahun[1])])
    }
    kontribusi[nrow(totalpdrb)-1,1]=kontribusi[nrow(totalpdrb)-1,1]/(nrow(totalpdrb)-2)
    kontribusi<-cbind(dataframe[1:30,1],kontribusi)
    names(kontribusi)=c("nama.wilayah","nilai_kontribusi_dari_seluruh_sektor")
    rataKontribusiSemuaSektor<-kontribusi
}

#UPDATE:
rataKontribusiSemuaSektor_untukdisertasi <- function(dataframe=dataPDRB,range_tahun=2011:2015, vektor_index=c()) {
    totalpdrb<-totalPDRBpertahun(dataframe, vektor_index)
    #Ubah nama kolom ke tahun
    m<-c()
    for (i in 1:ncol(totalpdrb)){
        m<-append(m,substr(names(totalpdrb[i]),13,16))
    }
    names(totalpdrb)=m
    
    #Hitung total PDRB dari seluruh totalPDRB setiap seluruh kabupaten dan provinsi
    total<-data.frame()
    for(j in 1:ncol(totalpdrb)){
        total[1,j]=0
        for(i in 1:(nrow(totalpdrb)-2)){
            total[1,j]=total[1,j]+totalpdrb[i,j]
        }
    }
    names(total)=m
    #print(total)
    
    #Hitung kontribusi total setiap kabupaten dan provinsi
    kontribusi<-data.frame()
    for(i in 1:(nrow(totalpdrb)-1)){
        #kontribusi[i,1]=(totalpdrb[i,toString(range_tahun[length(range_tahun)])]+totalpdrb[i,toString(range_tahun[1])])/(total[1,toString(range_tahun[length(range_tahun)])]+total[1,toString(range_tahun[1])])
        #berdasarkan data excel langsung:
        kontribusi[i,1]=round((totalpdrb[i,toString(range_tahun[length(range_tahun)])]+totalpdrb[i,toString(range_tahun[1])])/(87401819.62+87561647.57),digits = 3)
    }
    
    #Hitung kontribusi provinsi:
    kontribusi[nrow(totalpdrb)-1,1]=0.015
    #kontribusi[nrow(totalpdrb)-1,1]=kontribusi[nrow(totalpdrb)-1,1]/(nrow(totalpdrb)-2)
    kontribusi<-cbind(dataframe[1:30,1],kontribusi)
    names(kontribusi)=c("nama.wilayah","nilai_kontribusi_dari_seluruh_sektor")
    rataKontribusiSemuaSektor<-kontribusi
}

#==================================================================================================================================
analisisKlassen_Seluruh_Sektor <- function(dataframe=dataPDRB,indexWilayah=1:29,indexAcuan=30,range_tahun=2011:2015,vektor_index=list()) {
    pertumbuhan<-rataPertumbuhanSemuaSektor(dataframe,range_tahun,vektor_index)
    kontribusi<-rataKontribusiSemuaSektor(dataframe,range_tahun,vektor_index)
    
    #Analisis Klassen untuk seluruh sektor di tiap kabupaten, sama dengan menghitung kawasan andalan, bukan lagi sektor andalan
    kelas<-data.frame()
    for(i in indexWilayah){
        if((pertumbuhan[i,2]>=pertumbuhan[indexAcuan,2])&&(kontribusi[i,2]>=kontribusi[indexAcuan,2])){kelas[i,"Kuadran_Wilayah"]="K1"}
        else if((pertumbuhan[i,2]<pertumbuhan[indexAcuan,2])&&(kontribusi[i,2]>=kontribusi[indexAcuan,2])){kelas[i,"Kuadran_Wilayah"]="K2"}
        else if((pertumbuhan[i,2]>=pertumbuhan[indexAcuan,2])&&(kontribusi[i,2]<kontribusi[indexAcuan,2])){kelas[i,"Kuadran_Wilayah"]="K3"}
        else {kelas[i,"Kuadran_Wilayah"]="K4"}
    }
    
    #Tambahkan kolom nama-nama kabupaten
    kelas<-cbind(dataframe[indexWilayah,1],kelas)
    names(kelas)<-c("nama.wilayah","kuadran_wilayah")
    analisisKlassen_Seluruh_Sektor<-kelas
}
#==================================================================================================================================
data_semua_sektor_tahun_tertentu <-function(dataframe=dataPDRB,tahun=2011,vektor_index=list()){
    if(length(vektor_index)==0){index_semua_sektor<-buatindex()}else{index_semua_sektor<-buatindex(vektor_index)}
    m<-data.frame()
    
    for(name_index in names(index_semua_sektor) ){
        v<-unlist(strsplit(name_index,"[r]"))
        #p<-paste(v[2],v[3],sep="_")
        #print(p)
        if(v[2]==toString(tahun)){
            for(i in 1:nrow(dataframe)){
                kolom<-1
                for(j in index_semua_sektor[,name_index]){
                    m[i,paste("S",toString(kolom),"_",toString(tahun),sep="")]=dataframe[i,j]
                    kolom=kolom+1    
                }
            }
        break()    
        }
    }
    m<-cbind(dataframe[,1],m)
    #print(colnames(m[,2:9]))
    names(m)=c("nama.wilayah",colnames(m[,2:10]))
    data_semua_sektor_tahun_tertentu<-m
}
#==================================================================================================================================
analisisShiftshare <-function(Ekab1,Ekab2,Eprov1,Eprov2) {
    m<-data.frame()
    for(i in 1:length(Ekab1)){
        #Hitung ProvinicialShare:
        m[i,"PS"] <- Ekab1[i]*((sum(Eprov2)/sum(Eprov1))-1)
        #Hitung ProportionalShift:
        m[i,"P"] <- Ekab1[i]*((Eprov2[i]/Eprov1[i])-(sum(Eprov2)/sum(Eprov1)))
        #Hitung DifferentialShift:
        m[i,"DS"] <- Ekab2[i]-((Eprov2[i]/Eprov1[i])*Ekab1[i])
        #Hitungan Pertambahan nilai = PS + P + DS
        m[i,"Delta"] <- m[i,"PS"] + m[i,"P"] + m[i,"DS"]
        
    }
    analisisShiftshare<-m
}
#==================================================================================================================================
analisisShiftsharetotal_DS <-function(datapdrb=dataPDRB,range_tahun=2011:2012, vektor_index=list()) {
    #Hasilkan data untuk tahun awal dan tahun akhir
    m1<-data.frame()
    m2<-data.frame()
    m1<-(m1<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[1],vektor_index))[,2:10]
    m2<-(m2<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[length(range_tahun)],vektor_index))[,2:10]
    
    m<-data.frame()
    for(i in 1:nrow(m1)){
        DS<-(analisisShiftshare(t(m1[i,1:9]),t(m2[i,1:9]),t(m1[30,1:9]),t(m2[30,1:9])))[,"DS"]
        for(j in 1:9){
            m[i,j]<-DS[j]
        }
    }
    names(m)=c("DS_S1","DS_S2","DS_S3","DS_S4","DS_S5","DS_S6","DS_S7","DS_S8","DS_S9")
	m<-cbind(kabupaten=datapdrb[,1],m)
    analisisShiftsharetotal<-m
    
}

#==================================================================================================================================
analisisShiftsharetotal_PS <-function(datapdrb=dataPDRB,range_tahun=2011:2012, vektor_index=list()) {
    #Hasilkan data untuk tahun awal dan tahun akhir
    m1<-data.frame()
    m2<-data.frame()
    m1<-(m1<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[1],vektor_index))[,2:10]
    m2<-(m2<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[length(range_tahun)],vektor_index))[,2:10]
    
    m<-data.frame()
    for(i in 1:nrow(m1)){
        PS<-(analisisShiftshare(t(m1[i,1:9]),t(m2[i,1:9]),t(m1[30,1:9]),t(m2[30,1:9])))[,"PS"]
        for(j in 1:9){
            m[i,j]<-PS[j]
        }
    }
    names(m)=c("PS_S1","PS_S2","PS_S3","PS_S4","PS_S5","PS_S6","PS_S7","PS_S8","PS_S9")
    m<-cbind(kabupaten=datapdrb[,1],m)
    analisisShiftsharetotal<-m
    
}
#==================================================================================================================================
analisisShiftsharetotal_P <-function(datapdrb=dataPDRB,range_tahun=2011:2012, vektor_index=list()) {
    #Hasilkan data untuk tahun awal dan tahun akhir
    m1<-data.frame()
    m2<-data.frame()
    m1<-(m1<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[1],vektor_index))[,2:10]
    m2<-(m2<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[length(range_tahun)],vektor_index))[,2:10]
    
    m<-data.frame()
    for(i in 1:nrow(m1)){
        P<-(analisisShiftshare(t(m1[i,1:9]),t(m2[i,1:9]),t(m1[30,1:9]),t(m2[30,1:9])))[,"P"]
        for(j in 1:9){
            m[i,j]<-P[j]
        }
    }
    names(m)=c("P_S1","P_S2","P_S3","P_S4","P_S5","P_S6","P_S7","P_S8","P_S9")
    m<-cbind(kabupaten=datapdrb[,1],m)
    analisisShiftsharetotal<-m
    
}
#==================================================================================================================================
analisisShiftsharetotal_Delta <-function(datapdrb=dataPDRB,range_tahun=2011:2012, vektor_index=list()) {
    #Hasilkan data untuk tahun awal dan tahun akhir
    m1<-data.frame()
    m2<-data.frame()
    m1<-(m1<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[1],vektor_index))[,2:10]
    m2<-(m2<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[length(range_tahun)],vektor_index))[,2:10]
    
    m<-data.frame()
    for(i in 1:nrow(m1)){
        Delta<-(analisisShiftshare(t(m1[i,1:9]),t(m2[i,1:9]),t(m1[30,1:9]),t(m2[30,1:9])))[,"Delta"]
        for(j in 1:9){
            m[i,j]<-Delta[j]
        }
    }
    names(m)=c("Delta_S1","Delta_S2","Delta_S3","Delta_S4","Delta_S5","Delta_S6","Delta_S7","Delta_S8","Delta_S9")
    m<-cbind(kabupaten=datapdrb[,1],m)
    analisisShiftsharetotal_Delta<-m
    
}

#==================================================================================================================================
#Sesuai Disertasi Khusus Rentang Tahun 2011:2012
analisisShiftshare_disertasi2011_2012 <- function(Ekab1,Ekab2,Eprov1=c(12009983,49071835,2185412,31917,8699020,7364731,3791014,1333170,912506),Eprov2=c(12641425,45658017,2269227,35431,9809683,7975604,4117302,1431576,994707)) {
    m<-data.frame()
    for(i in 1:length(Ekab1)){
        #Hitung ProvinicialShare:
        m[i,"PS"] <- Ekab1[i]*((sum(Eprov2)/sum(Eprov1))-1)
        #Hitung ProportionalShift:
        m[i,"P"] <- Ekab1[i]*((Eprov2[i]/Eprov1[i])-(sum(Eprov2)/sum(Eprov1)))
        #Hitung DifferentialShift:
        m[i,"DS"] <- Ekab2[i]-((Eprov2[i]/Eprov1[i])*Ekab1[i])
        #Hitungan Pertambahan nilai = PS + P + DS
        m[i,"Delta"] <- m[i,"PS"] + m[i,"P"] + m[i,"DS"]
        
    }
    #names(m)=c("nama.wilayah","S1","S2","S3","S4","S5","S6","S7","S8","S9")
    m<-cbind(kabupaten=datapdrb[,1],m)
    analisisShiftshare<-m
}

analisisShiftsharetotal_DS_disertasi_2011_2012
function(datapdrb=dataPDRB,range_tahun=2011:2012, vektor_index=list()) {
    #Hasilkan data untuk tahun awal dan tahun akhir
    m1<-data.frame()
    m2<-data.frame()
    m1<-(m1<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[1],vektor_index))[,2:10]
    m2<-(m2<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[length(range_tahun)],vektor_index))[,2:10]
    
    m<-data.frame()
    for(i in 1:nrow(m1)){
        DS<-(analisisShiftshare_disertasi2011_2012(t(m1[i,1:9]),t(m2[i,1:9])))[,"DS"]
        for(j in 1:9){
            m[i,j]<-DS[j]
        }
    }
    names(m)=c("DS_S1","DS_S2","DS_S3","DS_S4","DS_S5","DS_S6","DS_S7","DS_S8","DS_S9")
    m<-cbind(kabupaten=datapdrb[,1],m)
    analisisShiftsharetotal<-m
    
}

#==================================================================================================================================
analisisShiftshare_Kabupaten <-
function(Total_sektor_kab_t1,Total_sektor_kab_t2,Total_sektor_prov_t1,Total_sektor_prov_t2) {
    m<-data.frame()
    for(i in 1:length(Total_sektor_kab_t1)){
        #Hitung ProvinicialShare:
        m[i,"PS"] <- Total_sektor_kab_t1[i]*((sum(Total_sektor_prov_t2)/sum(Total_sektor_prov_t1))-1)
        #Hitung ProportionalShift:
        m[i,"P"] <- Total_sektor_kab_t1[i]*((Total_sektor_prov_t2[i]/Total_sektor_prov_t1[i])-(sum(Total_sektor_prov_t2)/sum(Total_sektor_prov_t1)))
        #Hitung DifferentialShift:
        m[i,"DS"] <- Total_sektor_kab_t2[i]-((Total_sektor_prov_t2[i]/Total_sektor_prov_t1[i])*Total_sektor_kab_t1[i])
        #Hitungan Pertambahan nilai = PS + P + DS
        m[i,"Delta"] <- m[i,"PS"] + m[i,"P"] + m[i,"DS"]
        
    }
    analisisShiftshare<-m
}


analisisShiftsharetotal_DS_kabupaten <- function(datapdrb=dataPDRB,range_tahun=2011:2012, vektor_index=list()) {
    #Hasilkan data untuk tahun awal dan tahun akhir
    m1<-data.frame()
    m2<-data.frame()
    m1<-(m1<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[1],vektor_index))[,2:10]
    m2<-(m2<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[length(range_tahun)],vektor_index))[,2:10]
    
    #Hitung kolom total pendapatan seluruh sektor perkabupaten
    for(i in 1:nrow(m1)) {
        m1[i,"total_S"]=sum(m1[i,1:9])
        m2[i,"total_S"]=sum(m2[i,1:9])
    }
    
    DS<-data.frame()
    for(i in 1:nrow(m1)){
        DS[i,"DS kabupaten"]<-(analisisShiftshare_Kabupaten(m1[i,10],m2[i,10],m1[30,10],m2[30,10]))[,"DS"]
    }
    
    DS<-cbind(datapdrb[,1],DS,rank(-DS[,"DS kabupaten"]))
    names(DS)=c("nama.wilayah","DS kabupaten","Ranking")
    analisisShiftsharetotal<-DS[1:29,]
    
}

analisisShiftsharetotal_DS_kabupaten_Disertasi_2011_2012 <-
function(datapdrb=dataPDRB,range_tahun=2011:2012, vektor_index=list()) {
    #Hasilkan data untuk tahun awal dan tahun akhir
    m1<-data.frame()
    m2<-data.frame()
    m1<-(m1<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[1],vektor_index))[,2:10]
    m2<-(m2<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[length(range_tahun)],vektor_index))[,2:10]
    
    #Hitung kolom total pendapatan seluruh sektor perkabupaten
    for(i in 1:nrow(m1)) {
        m1[i,"total_S"]=sum(m1[i,1:9])
        m2[i,"total_S"]=sum(m2[i,1:9])
    }
    
    DS<-data.frame()
    for(i in 1:nrow(m1)){
        DS[i,"DS kabupaten"]<-(analisisShiftshare_Kabupaten(m1[i,10],m2[i,10],c(85941769.65),c(85553803.21)))[,"DS"]
    }
    
    DS<-cbind(datapdrb[,1],DS,rank(-DS[,"DS kabupaten"]))
    names(DS)=c("nama.wilayah","DS kabupaten","Ranking")
    analisisShiftsharetotal_DS_kabupaten_Disertasi_2011_2012 <- DS[1:29,]
    
}
#==================================================================================================================================
analisisShiftsharetotal_PS_kabupaten <- function(datapdrb=dataPDRB,range_tahun=2011:2012, vektor_index=list()) {
    #Hasilkan data untuk tahun awal dan tahun akhir
    m1<-data.frame()
    m2<-data.frame()
    m1<-(m1<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[1],vektor_index))[,2:10]
    m2<-(m2<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[length(range_tahun)],vektor_index))[,2:10]
    
    #Hitung kolom total pendapatan seluruh sektor perkabupaten
    for(i in 1:nrow(m1)) {
        m1[i,"total_S"]=sum(m1[i,1:9])
        m2[i,"total_S"]=sum(m2[i,1:9])
    }
    
    PS<-data.frame()
    for(i in 1:nrow(m1)){
        PS[i,"PS kabupaten"]<-(analisisShiftshare_Kabupaten(m1[i,10],m2[i,10],m1[30,10],m2[30,10]))[,"PS"]
    }
    
    PS<-cbind(datapdrb[,1],PS,rank(-PS[,"PS kabupaten"]))
    names(PS)=c("nama.wilayah","PS kabupaten","Ranking")
    analisisShiftsharetotal<-PS[1:29,]
    
}
#==================================================================================================================================
analisisShiftsharetotal_P_kabupaten <- function(datapdrb=dataPDRB,range_tahun=2011:2012, vektor_index=list()) {
    #Hasilkan data untuk tahun awal dan tahun akhir
    m1<-data.frame()
    m2<-data.frame()
    m1<-(m1<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[1],vektor_index))[,2:10]
    m2<-(m2<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[length(range_tahun)],vektor_index))[,2:10]
    
    #Hitung kolom total pendapatan seluruh sektor perkabupaten
    for(i in 1:nrow(m1)) {
        m1[i,"total_S"]=sum(m1[i,1:9])
        m2[i,"total_S"]=sum(m2[i,1:9])
    }
    
    P<-data.frame()
    for(i in 1:nrow(m1)){
        P[i,"P kabupaten"]<-(analisisShiftshare_Kabupaten(m1[i,10],m2[i,10],m1[30,10],m2[30,10]))[,"P"]
    }
    
    P<-cbind(datapdrb[,1],P,rank(-P[,"P kabupaten"]))
    names(P)=c("nama.wilayah","P kabupaten","Ranking")
    analisisShiftsharetotal<-P[1:29,]
    
}
#==================================================================================================================================
analisisShiftsharetotal_Delta_kabupaten <- function(datapdrb=dataPDRB,range_tahun=2011:2012, vektor_index=list()) {
    #Hasilkan data untuk tahun awal dan tahun akhir
    m1<-data.frame()
    m2<-data.frame()
    m1<-(m1<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[1],vektor_index))[,2:10]
    m2<-(m2<-data_semua_sektor_tahun_tertentu(datapdrb,range_tahun[length(range_tahun)],vektor_index))[,2:10]
    
    #Hitung kolom total pendapatan seluruh sektor perkabupaten
    for(i in 1:nrow(m1)) {
        m1[i,"total_S"]=sum(m1[i,1:9])
        m2[i,"total_S"]=sum(m2[i,1:9])
    }
    
    Delta<-data.frame()
    for(i in 1:nrow(m1)){
        Delta[i,"Delta kabupaten"]<-(analisisShiftshare_Kabupaten(m1[i,10],m2[i,10],m1[30,10],m2[30,10]))[,"Delta"]
    }
    
    Delta<-cbind(datapdrb[,1],Delta,rank(-Delta[,"Delta kabupaten"]))
    names(Delta)=c("nama.wilayah","Delta kabupaten","Ranking")
    analisisShiftsharetotal<-Delta[1:29,]
    
}
#==================================================================================================================================
hitungBorda_kawasanAndalan <- function(dataframe=datakuesioner_kawasanAndalan,bobot_peringkat=c(4,3,2,1),kab=list("Biak","Boven Digul","Kab Jayapura","Jayawijaya","Kerom","Merauke","Mimika","Nabire","Paniai","Sarmi","Yapen","Kota Jayapura")) {
    
    #Hitung nilai skor borda untuk tiap-tiap kabupaten
    hitung<-data.frame()
    sum<-0
    k<-1
    for(i in kab) {
            hitung[i,"point Borda"]<-(dataframe[k,2]*bobot_peringkat[1])+(dataframe[k,3]*bobot_peringkat[2])+(dataframe[k,4]*bobot_peringkat[3])+(dataframe[k,5]*bobot_peringkat[4])
            sum<-sum+hitung[i,"point Borda"]
        k<-k+1
    }
    g<-c()
    for(i in kab){
        hitung[i,"nilai Borda"]<-hitung[i,"point Borda"]/sum
        g<-c(g,hitung[i,"nilai Borda"])
    }
    h<-rank(-g, ties.method = "max")
    
    j<-1
    for(i in kab){
        hitung[i,"rangking"]<-h[j]
        j<-j+1
    }
    hitungBorda_sektorUnggulan_perKAb<-hitung
}
#==================================================================================================================================

#==================================================================================================================================
#==================================================================================================================================
#==================================================================================================================================

