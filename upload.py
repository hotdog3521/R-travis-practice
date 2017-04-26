import dropbox
import sys



information_list = []
file = open("information.txt", "r")
for line in file:
	information_list.append(line)
file.close()

client = dropbox.client.DropboxClient(information_list[0].strip())
print 'linked account : ', client.account_info()

f = open(information_list[1], 'rb')
response = client.put_file('/UploadingPractice/'+information_list[1],f)
print 'uploaded:', response






