import dropbox
import sys


file1 = open("output.txt", "r") 
pdffile = file1.readline().strip()
file1.close()

information_list = []
file = open("information.txt", "r")
for line in file:
	information_list.append(line)
file.close()

client = dropbox.client.DropboxClient(information_list[0].strip())
print 'linked account : ', client.account_info()

realfilename = pdffile[7:]
f = open(pdffile, 'rb')
response = client.put_file('/UploadingPractice/'+realfilename,f)
print 'uploaded:', response