#!/bin/bash
# Install TLS certificates

if [ ! -d $HOME/ssl ];
then
	mkdir $HOME/ssl
fi
if [ ! -f $HOME/ssl/cert.pem ];
then
	echo "Creating TLS certificates."
	openssl req -newkey rsa:2048 -nodes -x509 -days 1024 \
		-subj /C=LK/L=Colombo/O=SigScale/CN=ca.$(hostname)\/emailAddress=support@$(hostname) \
		-keyout $HOME/ssl/cakey.pem \
		-out $HOME/ssl/ca.pem
	openssl req -newkey rsa:2048 -nodes \
		-subj /C=LK/L=Colombo/O=SigScale/CN=$(hostname)\/emailAddress=support@$(hostname) \
		-keyout $HOME/ssl/key.pem \
		-out $HOME/ssl/cert.csr
	echo "extendedKeyUsage = serverAuth" > $HOME/ssl/extensions
	echo "subjectAltName = DNS:$(hostname)" >> $HOME/ssl/extensions
	openssl x509 -req -days 1024 \
		-CA $HOME/ssl/ca.pem \
		-CAkey $HOME/ssl/cakey.pem \
		-CAcreateserial \
		-extfile $HOME/ssl/extensions \
		-in $HOME/ssl/cert.csr \
		-out $HOME/ssl/cert.pem
	openssl x509 -outform DER \
		-in $HOME/ssl/ca.pem \
		-out $HOME/ssl/ca.der
	chmod 400 $HOME/ssl/key.pem $HOME/ssl/cakey.pem
else
	echo "Leaving existing TLS certificates in place."
fi

