# A Minimal Hosting Service

There are many cloud computing providers:
[DigitalOcean](https://www.digitalocean.com/),
[AWS](https://aws.amazon.com/),
[Azure](https://azure.microsoft.com),
[GCP](https://cloud.google.com/),
[IBM Cloud](https://cloud.ibm.com/catalog),
[Linode](https://www.linode.com/),
[Hetzner](https://www.hetzner.com/),
[OVH](https://www.ovh.com/).
To use these services,
one must set up and maintain complicated configuration files
and [IaC](https://en.wikipedia.org/wiki/Infrastructure_as_code) tools.
Large organizations can spend this effort.
But the effort is too much for small projects.

This document describes a cloud computing service
with the features needed by small projects
and a simple configuration interface.
It also describes some new services which current cloud services do not offer,
which can help small projects reduce their deployment and operations workload.

# UX

You can sign up, pay, and deploy your software using only the command-line tool `curl`.

First, create a TLS key and certificate.
You will use this to authenticate yourself to the service.
```
$ openssl req -newkey rsa:2048 -new -nodes -x509 -days 36500 -subj '/' -keyout identity.key -out identity.crt
$ cp identity.key identity.crt /Volumes/Backup/
```

Get the URL of the service's latest Terms of Use:
```
$ curl --silent --head https://api.example.com/terms |grep Location
Location: https://api.example.com/terms-19700101
```

Create a `config.toml` file:
```toml
schema = "e0-minimal-hosting-account"

[server]
cores = 1
ram-mb = 256
disk-mb = 100
inbound-tls = [
    ["johndoe.com", 443, 8443], # HTTPS
]
inbound-http = [
    ["johndoe.com", 8080],
]
inbound-dns = [
    ["johndoe.com", 8053, 8053],
]

[legal]
terms = "https://api.example.com/terms-19700101"
i-agree = true

[payment]
invoice-number = 1
amount = 20
currency = "USD"

[payment.credit-card-us]
name = "JOHN DOE"
number = "0000 0000 0000 0000"
expiration = 1970-01-01
security-code = "0000"
address = [
    "John Doe",
    "Doe LLC",
    "1 Example Rd Ste 1",
    "Example City, CA 00000"
]
```

To create your account, upload the file using your certificate.
You may re-upload to change settings or to authorize new payments.
Change the `invoice-id` field to authorize a new charge.

```
$ curl --cert identity.crt -X POST https://api.example.com/config --upload-file config.toml
Checked config.
Processing payment of USD 100.00 for invoice "1".
Saved invoice at https://api.example.com/invoices/1 .
Done.
```

Upload a secrets file:
```
$ curl --cert identity.crt -X PUT https://api.example.com/secrets --upload-file secrets.env
```

Finally, build and upload your server binary:
```
$ ./build.sh
$ curl --cert identity.crt -X PUT https://api.example.com/binary?type=e0-i32kvm --upload-file build/server.img
```

Watch your server's stdout:
```
$ curl --cert identity.crt -X STREAM https://api.example.com/jsonl?start=$(($(date +%s) - 60))
{"ts":1634609464532729,"th":"main","mod":"server","info":"Server starting.","branch":"main","commit":"08af4dd2839aad44dc9a5566b1ff236afa3de30f","tag":"release-120"}
{"ts":1634609464532731,"th":"main","mod":"server","info":"Loaded /params.toml."}
{"ts":1634609464532731,"params.dns_servers":["19.21.30.7","19.21.31.3"]}
{"ts":1634609464532731,"params.load_balancer_cname":"lb-1682921.example.com"}
{"ts":1634609464532782,"th":"main","mod":"server","info":"DNS server listening on UDP 53 and TCP 53."}
{"ts":1634609464532837,"th":"main","mod":"server","info":"Serving TCP 8080, HTTP"}
{"ts":1634609532602912,"th":"main","mod":"server","info":"CNAME lb-1682921.example.com has A records 19.21.29.172, 19.21.29.173"}
{"ts":1634609532602913,"th":"main","mod":"server","info":"CNAME lb-1682921.example.com has AAAA records 2001:db8::ff00:42:8329, 2001:db8::ff00:43:2417"}
{"ts":1634609532602913,"dns-record":"johndoe.com","dns-type":"A","dns-value":["19.21.29.172","19.21.29.173"]}
{"ts":1634609532602913,"dns-record":"johndoe.com","dns-type":"AAAA","dns-value":["2001:db8::ff00:42:8329","2001:db8::ff00:43:2417"]}
{"ts":1634609532602913,"dns-record":"johndoe.com","dns-type":"MX","dns-value":["1","aspmx.l.google.com"]}
{"ts":1634609532609714,"th":"main","mod":"server","info":"File not found: /data/certs/johndoe.com.crt"}
{"ts":1634609537717821,"th":"main","mod":"server","info":"Requested certificate for johndoe.com from ACME service https://api.buypass.com/acme/directory"}
{"ts":1634609537717821,"dns-record":"_acme-challenge.johndoe.com","dns-type":"TXT","dns-value":"gfj9Xq...Rg85nM"}
{"ts":1634609562085157,"th":"main","mod":"server","info":"Obtained certificate for johndoe.com"}
{"ts":1634609571594600,"th":"main","mod":"server","info":"Wrote file: /data/certs/johndoe.com.crt"}
{"ts":1634609571594611,"th":"main","mod":"server","info":"Serving TCP 8443, TLS johndoe.com, HTTP"}
^C
```

# API

## mTLS

## HTTP PUT

## Toml Config
```toml
type = "e0-minimal-hosting-account"

[server]
cores = 1
ram-mb = 256
disk-mb = 100
maintenance-start-utc = 10:00:00
inbound-http = [
    ["johndoe.com", 8080],
]
inbound-tls = [
    ["johndoe.com", 443, 8443], # HTTPS
    ["www.johndoe.com", 443, 8443], # HTTPS
    ["mail.johndoe.com", 465, 8465], # SMTP
]
inbound-dns = [
    ["johndoe.com", 8053, 8053],
]
inbound-ssh-public-keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCmuC02GkkkvQLl87fPdqEmzzyCPSFqauUkWCR1NJ0YuGM6qYT8+Dh0dhlAoJr5UWuBUmxuS9vjHTiWrP0d4/R8mbrfMHOyWrUlssjMw5UR+uvbUo/QN54p5y5nPQ3sooQpD7RcyRwPURBeA9p1oAfIddfWrEUZ/f8A7TKP9i+0fKNGEJrGH3N9CsLOXQDMP3K8UJfWaG6vTWtMoasvSD+g2+BVz6mlbiOg+kYGhrrsBcWdJ8hv4vcTKj99nvgGrzbUh2Gww0hSh7j1mBnHk2DcLF922JjnyO/uErQdpgsMqucQrSzhYPdoyhqJnBRx09GyVuCvKkdNEfbjT0XVvy0FgZsVw9AMnwMblGj6+C3hgtZLIZYEVD20gwkHbb8udI5yr9hcTaXH5knIgbaWZjCnMwv/sTOR+gQbl9llk1Taco7EAxcpOKz+tKARKOl0l9NwkdULuSvIuykgEhFe3zi/P/9mLz4J/DH++EDGSUFcz2jvDAPpRcHmX5fasDyOZTk= user@laptop",
]
outbound = [
    ["api.example-sms-service.com", "tcp", 443],
    ["api.example-email-service.com", "tcp", 443],
]

[legal]
terms = "https://api.example.com/terms-19700101"
i-agree = true

[payment]
invoice-number = 1
amount = 20
currency = "USD"

[payment.credit-card-us]
name = "JOHN DOE"
number = "0000 0000 0000 0000"
expiration = 1970-01-01
security-code = "0000"
address = [
    "John Doe",
    "Doe LLC",
    "1 Example Rd Ste 1",
    "Example City, CA 00000"
]
```

## Binary
To stop the server, upload a zero-length binary.

## Secrets

## Backup & Restore

# Network

## TLS Reverse Proxy

## DNS Reverse Proxy

# Preventing Abuse

## Outbound Allowlist

## Prepayment

## Monitoring

### JSON Metrics

### Hosting Service Metrics

