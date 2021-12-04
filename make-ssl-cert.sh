## make-ssl-cert.sh --- create a self-signed SSL certificate

set -x

# Host name.
host=$(hostname -s)
# Domain name.
domain=$(hostname -d)
# Fully qualified host name.
full=$host.$domain
# IP address.
addr=$(hostname -I | tr ' ' '\n' | grep -v ':')
# Output file name.
out=example
# Secret key file name.
key=$out.key
# Certificate file name.
cert=$out.pem

openssl req -x509 -newkey rsa:4096 -sha256 -days 3650 \
	-nodes -keyout $key -out $cert \
	-subj /CN=$full \
	-addext subjectAltName=DNS:$full,DNS:$domain,DNS:localhost,IP:$addr,IP:127.0.0.1

# Review the certificate.
openssl x509 -text -noout -in $cert

## make-ssl-cert.sh ends here
