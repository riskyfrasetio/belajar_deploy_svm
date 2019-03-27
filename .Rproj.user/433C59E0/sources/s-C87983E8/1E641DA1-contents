library(RMySQL)
library(dplyr)
library(data.table)
library(tidyverse)
library(ggplot2)
library(gower)
library(Biocomb)
con <- dbConnect(RMySQL::MySQL(), dbname = "update08_03",host="localhost",port=3306,username="root",password="")
dbListTables(con)
tb.proposal=tbl(con,"gt_proposal") %>% select(id,borrower_id) %>% filter(id>=2023)
tb.borrower=tbl(con,"gt_borrower") %>% select(-created_at,-updated_at,-status)
tb.lokasi=tbl(con,"gt_districts") %>% select(id,name)
tb.baru=tb.borrower %>% left_join(tb.lokasi,by=c("kecamatan_usaha"="id")) %>% select(-kecamatan_usaha)
tb.angsuran.detail=tbl(con,"gt_angsuran_detail") %>% select(-id,-user_id,-created_at,-updated_at,-status) 
tb.ang.pr=tb.angsuran.detail %>% left_join(tb.proposal,by=c("proposal_id"="id"))
join.ba=tb.ang.pr %>% left_join(tb.baru,by=c("borrower_id"="id"))
kol=colnames(join.ba)
join.ba.1=join.ba %>% select(-user_id,-nama_depan,-nama_belakang,-email,-tempat_lahir,-telpon_pribadi,-ktp,-foto,-provinsi_pribadi,-kota_pribadi,-kecamatan_pribadi,-kelurahan_pribadi,-income_tambahan,-expense_rt,-jumlah_pinjaman,-cicilan_pinjaman,-key_token,-score_visit,-agree_tnc,-default,-deskripsi_usaha,-alasan_usaha,-jenis_pelatihan,-type,-step1,-step2,-step3,-step4,-deleted_at,-badan_hukum,-kode_penyelenggara,-jenis_layanan,-alamat_penyelenggara,-agama,-info_kredit,-tanggal_tawar,-jaminan,-jumlah_tawar_approved,-jenis_bayar,-status_pinjaman,-company_id) %>% filter(mitra=="gandengtangan")
data.ba=as.data.table(join.ba.1)
data.ba$tanggal_lahir=as.Date(data.ba$tanggal_lahir)
data.ba$tanggal_angsuran=as.Date(data.ba$tanggal_angsuran)
data.ba$tanggal_bayar=as.Date(data.ba$tanggal_bayar)
data.ba=data.ba[tanggal_angsuran <= "2019-03-08" | tanggal_bayar<= "2019-03-08" ]
tb.angsuran=tbl(con,"gt_angsuran") %>% select(-id,-created_at,-updated_at) 
tb.angsuran=as.data.table(tb.angsuran)
tb.angsuran=tb.angsuran[proposal_id %in% unique(data.ba$proposal_id)]
#mengambil berapa kali angsuran
jangka=c()
jangka.angsuran=c()
for (i in unique(data.ba$proposal_id)){
  g=tb.angsuran[proposal_id==i,max(angsuran_no)]
  h=rep(g,nrow(data.ba[proposal_id==i]))
  jangka.angsuran=c(jangka.angsuran,h)
}
data.ba=data.ba[,jangka.angsuran:=jangka.angsuran]
#mengambil proposal_id yang beres pembayarannya telah selesai
id=c()
for (i in unique(data.ba$proposal_id)){
  tanggal_angsuran.terakhir=unique(data.ba[proposal_id==i][angsuran_no==unique(jangka.angsuran),tanggal_angsuran])
  if (isTRUE(tanggal_angsuran.terakhir<="2019-02-14")){
    gg=i
    id=c(id,gg)
  }}
data.ba=data.ba[proposal_id %in% id]
data.ba=data.ba[,selisih.tenggat:=data.ba$tanggal_bayar-data.ba$tanggal_angsuran]
data.ba=data.ba[,status.pembayaran:= ifelse(selisih.tenggat > 0 | is.na(selisih.tenggat), "lambat", ifelse(selisih.tenggat == 0,"tepat","cepat"))]
data.ba[,.(angsuran=sum(amount)),by=.(angsuran_no,proposal_id,score_interview,score_gt,status.pembayaran)]
data.ba=data.ba[,jarak.angsuran:= ifelse(period_id==0 & method==1,7,ifelse(period_id==0 & method==2,14,ifelse(period_id==2 & method==2,7,14)))]
data.ba=data.ba[,status.kredit:= ifelse((selisih.tenggat-2*jarak.angsuran) >= 0 | is.na(selisih.tenggat-2*jarak.angsuran),"bermasalah","tidak")]
unique(data.ba[nama=="Dede Wahto",.(proposal_id,angsuran_no,tanggal_angsuran,tanggal_bayar,selisih.tenggat,status.pembayaran,status.kredit,tenor=jangka.angsuran,jarak.angsuran)],by="angsuran_no")
a=length(unique(data.ba$proposal_id))
tepat=c()
cepat=c()
lambat=c()
tenor=c()
durasi.lambat=c()
status.kredit=c()
for (i in unique(data.ba$proposal_id)){
  hh=nrow(data.ba[proposal_id==i,.(sum(amount)),by=.(proposal_id,angsuran_no,score_interview,score_gt,status.pembayaran)][status.pembayaran=="tepat"])
  ii=nrow(data.ba[proposal_id==i,.(sum(amount)),by=.(proposal_id,angsuran_no,score_interview,score_gt,status.pembayaran)][status.pembayaran=="lambat"])
  jj=nrow(data.ba[proposal_id==i,.(sum(amount)),by=.(proposal_id,angsuran_no,score_interview,score_gt,status.pembayaran)][status.pembayaran=="cepat"])
  ll=nrow(data.ba[proposal_id==i,.(sum(amount)),by=.(proposal_id,angsuran_no,score_interview,score_gt,status.pembayaran)])
  stat=ifelse(nrow(data.ba[proposal_id==i,.(sum(amount),status.kredit)][status.kredit=="bermasalah"])>0,"bermasalah","tidak")
  dur=as.numeric(sum(unique(data.ba[proposal_id==i,.(proposal_id,angsuran_no,tanggal_angsuran,tanggal_bayar,selisih.tenggat,status.pembayaran,status.kredit,tenor=jangka.angsuran,jarak.angsuran)],by="angsuran_no")[status.pembayaran=="lambat",selisih.tenggat],na.rm = TRUE))
  tepat=c(tepat,hh)
  lambat=c(lambat,ii)
  cepat=c(cepat,jj)
  tenor=c(tenor,ll)
  status.kredit=c(status.kredit,stat)
  durasi.lambat=c(durasi.lambat,dur)
}
data.olah=data.frame(proposal_id=unique(data.ba$proposal_id),tepat=tepat,cepat=cepat,lambat=lambat,tenor=tenor,kredit.bermasalah=status.kredit,durasi.lambat=durasi.lambat)
data.olah2=unique(data.ba, by="proposal_id", fromLast=TRUE)[,.(proposal_id,borrower_id,nik,nama,kecamatan=name,profit,pendidikan,pekerjaan,jenis_usaha,facebook_pribadi,twitter_pribadi,instagram_pribadi,facebook_usaha,twitter_usaha,instagram_usaha,fisik_usaha,income,expense,total_aset,purpose,tanggungan,rekening_id,jenis_kelamin,installment,proposed_loan,no_siup,price,tanggal_lahir,score_gt,score_interview)]
data.olah2=data.olah2[,-c("proposal_id")]
data.olah=as.data.table(data.olah)
data.proses=cbind(data.olah,data.olah2)
grade.score.interview=ifelse(data.proses$score_interview<=10,"good","bad")
grade.score.gt=ifelse(data.proses$score_gt>=55,"good","bad")
data.proses=data.proses[, c("grade.score.gt"):=list(grade.score.gt)]
data.proses$usia=2019-year(data.proses$tanggal_lahir)
data.proses$persen.lambat=round((data.proses$lambat/data.proses$tenor)*100,3)
data.proses$persen.cepat=round((data.proses$cepat/data.proses$tenor)*100,3)
data.proses$profit=data.proses$profit/1000000
tb_borrower_scoring=tbl(con,"gt_borrower_scoring") %>% select(borrower_id,scoring_id,scoring_detail_id)
tb.credit_scoring_detail=tbl(con,"gt_credit_scoring_detail") %>% select(scoring_id,scoring_detail_id,nama_scoring_detail)
data.bor.scoring=tb_borrower_scoring %>% filter(borrower_id %in% data.proses$borrower_id)
tb.credit_scoring=tbl(con,"gt_credit_scoring") %>% select(id,nama_id)
data.score=left_join(data.bor.scoring,tb.credit_scoring,by=c("scoring_id"="id"))
data.bor.score=left_join(data.score,tb.credit_scoring_detail,by=c("scoring_id"="scoring_id","scoring_detail_id"="scoring_detail_id")) %>% select(borrower_id,nama_id,nama_scoring_detail)
data.bor.score=as.data.table(data.bor.score)
data.scoring=data.bor.score %>% spread(nama_id, nama_scoring_detail, fill = NA, convert = FALSE) %>% select(-borrower_id)
setnames(data.scoring,"usia","grade.usia")
setnames(data.scoring,"pendidikan","grade.pendidikan")
data.konsol=cbind(data.proses,data.scoring)

#durasi lambat belum selesai
data.konsol[,.N,by=.(nik)][N>=2,nik]
duplikat.nik=data.konsol[,.N,by=.(nik)][N>=2,nik]
duplikat.nik.status=ifelse(data.konsol$nik %in% duplikat.nik,"ada","tidak ada")
data.konsol$duplikat.nik.status=duplikat.nik.status

# proses generate tabel status riwayat kredit
stat=data.konsol[,.(proposal_id,nik,durasi.lambat,kredit.bermasalah,duplikat.nik.status)]
data.riwayat.kredit=data.table()
k=data.table()
proposal_id=c()
for (i in unique(stat$nik)){
  g=stat[nik==i]
  if (nrow(g)==1){
    proposal_id=g[,proposal_id]
    k=cbind(proposal_id=proposal_id,nik=i,riwayat.kredit="tidak ada riwayat")
  }  else {
    r=rep(NA,nrow(g))
    r[1]="tidak ada riwayat"
    nik=rep(i,nrow(g))
    proposal_id[1]=g[1,proposal_id]
    for (j in 2:nrow(g)){
      #r[j]=ifelse(g[j-1,kredit.bermasalah]=="bermasalah" | g[j-1,durasi.lambat]> 30,"bermasalah","riwayat aman")
      r[j]=ifelse(g[j-1,kredit.bermasalah]=="bermasalah" | g[j-1,durasi.lambat]> 15,"bermasalah","riwayat aman")
      proposal_id[j]=g[j,proposal_id]
    }
    k=cbind(proposal_id=proposal_id,nik=nik,riwayat.kredit=r)
  }
  data.riwayat.kredit=rbind(data.riwayat.kredit,k)
}

data.riwayat.kredit[,proposal_id:=as.numeric(data.riwayat.kredit$proposal_id)]
data.konsol=data.konsol %>% select(-nik) %>% left_join(data.riwayat.kredit,by="proposal_id")
data.konsol=as.data.table(data.konsol)
persen.tidak=(nrow(data.konsol[kredit.bermasalah=="tidak"])/(nrow(data.konsol)))*100
persen.bermasalah=(nrow(data.konsol[kredit.bermasalah=="bermasalah"])/(nrow(data.konsol)))*100
data.konsol$grade.persen.lambat=ifelse(data.konsol$persen.lambat>=50,">=50 persen","<50persen")
data.konsol$jenis_kelamin=as.factor(data.konsol$jenis_kelamin)
levels(data.konsol$jenis_kelamin)=c("Pria","Wanita")
data.konsol$jumlahkamar=as.factor(data.konsol$jumlahkamar)
data.konsol$jumlahkendaraanpribadi=as.factor(data.konsol$jumlahkendaraanpribadi)
data.konsol$anggotakeluargabekerja=as.factor(data.konsol$anggotakeluargabekerja)
data.konsol$anggotakeluargabelumbekerja=as.factor(data.konsol$anggotakeluargabelumbekerja)
data.konsol$anggotakeluargabelumbekerja2=as.factor(data.konsol$anggotakeluargabelumbekerja2)
data.konsol$jenis_usaha=ifelse(data.konsol$jenis_usaha=="","other",data.konsol$jenis_usaha)
data.konsol$jenis_usaha=as.factor(data.konsol$jenis_usaha)
data.konsol$lamausaha=as.factor(data.konsol$lamausaha)
data.konsol$izinusaha=as.factor(data.konsol$izinusaha)
data.konsol$pembukuanusaha=as.factor(data.konsol$pembukuanusaha)
data.konsol$jumlahkaryawan=as.factor(data.konsol$jumlahkaryawan)
data.konsol$tipetempatkerja=as.factor(data.konsol$tipetempatkerja)
data.konsol$pendapatanperbulan=as.factor(data.konsol$pendapatanperbulan)
data.konsol$oer=(data.konsol$expense/data.konsol$income)*100
data.konsol$loan.ratio=(data.konsol$proposed_loan/data.konsol$total_aset)*100
data.konsol$income=data.konsol$income/1000000
data.konsol$pendapatansumberlain=as.factor(data.konsol$pendapatansumberlain)
data.konsol$jumlahtotalaset=as.factor(data.konsol$jumlahtotalaset)
data.konsol$rangenilaipinjaman=as.factor(data.konsol$rangenilaipinjaman)
data.konsol$cicilanpinjamanperbulan=as.factor(data.konsol$cicilanpinjamanperbulan)
data.konsol$jumlahkendaraanpribadi=as.factor(data.konsol$jumlahkendaraanpribadi)
data.konsol$jumlahanggotakeluarga=as.factor(data.konsol$jumlahanggotakeluarga)
data.konsol$grade.persen.lambat=as.factor(data.konsol$grade.persen.lambat)
data.konsol$statuspekerjaan=as.factor(data.konsol$statuspekerjaan)
data.konsol$fisik_usaha=as.factor(data.konsol$fisik_usaha)
data.konsol$proposed_loan=data.konsol$proposed_loan/1000000
data.konsol$pendidikanistri=as.factor(data.konsol$pendidikanistri)
data.konsol$lantairumah=as.factor(data.konsol$lantairumah)
data.konsol$installment=data.konsol$installment/1000
data.konsol$kulkas=as.factor(data.konsol$kulkas)
data.konsol$sumbermemasak=as.factor(data.konsol$sumbermemasak)
data.konsol$tabunggas12kg=as.factor(data.konsol$tabunggas12kg)
data.konsol$statusnikah=as.factor(data.konsol$statusnikah)
data.konsol$statusrumah=as.factor(data.konsol$statusrumah)
data.konsol$grade.pendidikan=as.factor(data.konsol$grade.pendidikan)
data.konsol$jarak=as.factor(data.konsol$jarak)
data.konsol$anggotakeluargasekolah=as.factor(data.konsol$anggotakeluargasekolah)
data.konsol$tanggungan=as.factor(data.konsol$tanggungan)
levels(data.konsol$tanggungan)=c("tidak ada","1-2 orang","3 orang atau lebih")
tb.rek=tbl(con,"gt_rekening") %>% select(id,nama_rekening,nama_bank)
tb.rek=as.data.table(tb.rek)
data.rek=data.konsol %>% left_join(tb.rek,by=c("rekening_id"="id"))
hh=strsplit(tolower(data.rek$nama_rekening)," ")
ii=strsplit(tolower(data.rek$nama)," ")
rek_nama=c()
for(i in 1:length(ii)){
  rek_nama[i]=any(ii[[i]] %in% hh[[i]])
}
data.rek$validasi.rekening[!rek_nama]="tidak sama"
data.rek$validasi.rekening[rek_nama]="sama"
data.rek$validasi.rekening=as.factor(data.rek$validasi.rekening)

#contoh untuk riwayat bermasalah
data.rek=as.data.table(data.rek)
data.rek[proposal_id %in% c(2197,2492)]
id.mas=data.rek[kredit.bermasalah=="tidak" & durasi.lambat>15,proposal_id]
data.rek[proposal_id %in% id.mas,kredit.bermasalah:="bermasalah"]

##
data.step2=data.rek %>% select(proposal_id,nik,profit,jenis_usaha,fisik_usaha,tenor,income,expense,tanggungan,jenis_kelamin,installment,proposed_loan,loan.ratio,price,score_gt,usia,anggotakeluargabekerja,anggotakeluargabelumbekerja,anggotakeluargabelumbekerja2,anggotakeluargasekolah,cicilanpinjamanperbulan,izinusaha,jarak,jumlahanggotakeluarga,jumlahkamar,jumlahkaryawan,total_aset,jumlahkendaraanpribadi,lamausaha,pembukuanusaha,pendapatanperbulan,pendapatansumberlain,grade.pendidikan,pendidikanistri,rangenilaipinjaman,statusnikah,statuspekerjaan,statusrumah,tabunggas12kg,tipetempatkerja,tipetoilet,oer,validasi.rekening,nama_bank,lantairumah,kulkas,kendaraanbermotor,sumbermemasak,riwayat.kredit,kredit.bermasalah)

data.step2$total_aset=data.step2$total_aset/1000000
jarak.angsuran=unique(data.ba[,c("proposal_id","jarak.angsuran")],by="proposal_id")[,jarak.angsuran]
jarak.angsuran=ifelse(jarak.angsuran==7,1,2)
#data.step2$tenor=data.step2$tenor*jarak.angsuran
#tenor g valid

##missing value
data.step2=as.data.table(data.step2)
na_count <-sapply(data.step2, function(y) sum(length(which(is.na(y)))))

## yang g dipake buat skoring menurut mas dwi
#jumlah anggota keluarga, anggota keluarga sekolah, pendidikan istri, status pekerjaan, lantai rumah, tipetoilet, sumbermemasak, tabunggas12 kg, kulkas, kendaraan bermotor.
data.step2=data.step2[,-c("jumlahanggotakeluarga","anggotakeluargasekolah","pendidikanistri","statuspekerjaan","lantairumah","tipetoilet","sumbermemasak","tabunggas12kg","kendaraanbermotor","kulkas","pendapatanperbulan","nama_bank","rangenilaipinjaman")]

data.step2$pendapatansumberlain=ifelse(is.na(data.step2$pendapatansumberlain),"tidak ada",data.step2$pendapatansumberlain)
data.step2$pendapatansumberlain=as.factor(data.step2$pendapatansumberlain)

## imputasi
#imputasi izin usaha untuk proposal id  menggunakan 3 tetangga 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
knn=gower_topn(data.step2[proposal_id==2259,-c("proposal_id","nik")],data.step2[proposal_id!=2259,-c("proposal_id","nik")])
knn$index
izin.usaha.ekstrapolasi=getmode(c(as.character(data.step2[knn$index[1],izinusaha]),as.character(data.step2[knn$index[2],izinusaha]),as.character(data.step2[knn$index[3],izinusaha])))
data.step2$izinusaha=as.factor(ifelse(is.na(data.step2$izinusaha),as.factor(izin.usaha.ekstrapolasi),data.step2$izinusaha))
levels(data.step2$izinusaha)=c("tidak ada","ada")
#imputasi score gt
id=data.step2[is.na(score_gt),proposal_id]
data.step2.tetangga=data.step2[!(proposal_id %in% id)]
data.step2.missing=data.step2[proposal_id %in% id]
data.step3=data.step2
for (i in id){
  knn.gt=gower_topn(data.step2.missing[proposal_id==i,-c("proposal_id","nik")],data.step2.tetangga[,-c("proposal_id","nik")])
  interpolasi.gt=mean(data.step2.tetangga[knn.gt$index[1:3],score_gt])
  data.step3$score_gt[data.step3$proposal_id==i]=interpolasi.gt
}
data.step3.olah=data.step3[,-c("proposal_id","nik","riwayat.kredit")]
data.step3.olah1=data.step3[,-c("proposal_id","nik","riwayat.kredit")]
data.step3.olah1$kredit.bermasalah=as.factor(ifelse(data.step3.olah$kredit.bermasalah=="bermasalah",1,0))
data.step3.olah1=data.step3.olah1[,-c("cicilanpinjamanperbulan","price","validasi.rekening"),with=FALSE]



library(MLmetrics)
Accuracy(predict(model.svm,data.step3.olah1[,-c("kredit.bermasalah"),with=FALSE]),data.step3.olah1[,kredit.bermasalah])
Sensitivity(predict(model.svm,data.step3.olah1[,-c("kredit.bermasalah"),with=FALSE]),data.step3.olah1[,kredit.bermasalah],positive="1")
Specificity(predict(model.svm,data.step3.olah1[,-c("kredit.bermasalah"),with=FALSE]),data.step3.olah1[,kredit.bermasalah],positive="1")








