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
with the features needed by small projects.
It has a simple configuration interface.
It also describes some new services which current cloud services do not offer,
which can help small projects reduce their deployment and operations workload.

# UX

You can sign up, pay, and deploy your software
using only the `openssl` command, the `curl` command, and a text editor.

First, create a TLS key and certificate.
You will use them to authenticate yourself to the service.
```
$ openssl req -newkey rsa:2048 -new -nodes -x509 -days 36500 -subj '/' -keyout identity.key -out identity.crt
$ cp identity.key identity.crt /Volumes/Backup/
```

Get the URL of the latest Terms of Use:
```
$ curl --silent --head https://api.example.com/terms |grep Location
Location: https://api.example.com/terms-19700101
```

Create a file:
```toml
# config.toml
schema = "minimal-hosting-account"

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
outbound = [
    ["api.buypass.com", "tcp", 443],
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

Upload the file to create the account and add money to its balance:
```
$ curl --cert identity.crt -X POST https://api.example.com/config --upload-file config.toml
Checked config.
Processing payment of USD 20.00 for invoice 1.
Saved invoice at https://api.example.com/invoices/1 .
Account balance is USD 20.00.
Done.
```

Finally, build and upload your server binary:
```
$ ./build.sh
$ curl --cert identity.crt -X PUT https://api.example.com/binary?type=x86-64-kvm --upload-file build/server.img
```

Watch your server's stdout:
```
$ curl --cert identity.crt -X STREAM https://api.example.com/jsonl
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

The API is HTTP REST over TLS.

We determine the URLs for each request by appending a path to the API URL.
Example API URL: `https://api.example.com/`.

## Create Key

You must authenticate yourself to the API.

1. Create a private key and a self-signed certificate:
   ```
   $ openssl req -newkey rsa:2048 -new -nodes -x509 -days 36500 -subj '/' -keyout identity.key -out identity.crt
   ```
2. Back up the files to an external drive:
   ```
   $ cp identity.key identity.crt /Volumes/Backup/
   ```
   Keep this copy of your certificate and key in a safe location.
   Without them, you cannot update your server or add money to your account.

Use the key and certificate whenever you make a request to the API.

When you call the API to create your account,
the API saves your certificate into your account.
When you make another request,
the API checks the provided certificate
and grants access to the account with the matching certificate.

## Create Account

1. Get the URL of the latest Terms of Use:
    ```
    $ curl --silent --head https://api.example.com/terms |grep Location
    Location: https://api.example.com/terms-19700101
    ```
2. Create a new `config.toml` file:
   ```toml
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
3. Upload the file:
   ```
   $ curl --cert identity.crt -X POST https://api.example.com/config --upload-file config.toml
   Checked config.
   Processing payment of USD 20.00 for invoice 1.
   Saved invoice at https://api.example.com/invoices/1 .
   Account balance is USD 20.00.
   Done.
   ```
   If the upload fails, try again.
   If you leave `invoice-number` unchanged, the API will not charge twice.

## Prepaid Balance

The hosting service is strictly prepaid.
When you let the account balance go to zero, you are closing the account.
The service stops your server and deletes its data.

You should set up monitoring and a low-balance alert.

The API may preserve your data for a limited time
to allow you to make a payment
and re-open the account.
This is called a "grace period".
During the grace period, the balance may become negative.

If you decide not to make a payment,
the hosting company must not try to collect any negative balance incurred during the grace period.

The hosting company may fail to deduct from the balance in a timely manner 
and unintentionally provide service to the account that is not covered by the prepaid balance.
The hosting company must treat this service as part of the grace period.

You control all charges.
You will never be charged more than the amount you pre-pay.

If you pre-pay, use the service, and then reverse the charge,
the hosting company can try to collect on your debt.

## Check Your Balance
The API generates `account-balance` metric records.
Retrieve your server's latest metrics and look for the last account balance value:
```
$ curl --cert identity.crt -X GET https://api.example.com/jsonl |grep '"account-balance"' |tail -n 1
{"ts":1634609464532731,"currency":"usd","account-balance":19.99}
```

## Add Money
To add money, edit `config.toml` and increment the `payment.invoice-number` field.
Then re-upload the file.

You can omit the `payment` and `payment.*`
sections from the file when you don't need to add money. 

## Download Invoice
To download invoice 1, perform an HTTP GET of `/invoice/1.txt` or `/invoice/1.pdf`.

Example:
```
$ curl --cert identity.crt -X GET https://api.example.com/invoice/1.pdf -o 1.pdf
```

## Configure Server
To configure the server, add a `server` section to the config file and upload it.

Example:
```toml
type = "minimal-hosting-account"

[server]
cores = 1
ram-mb = 256
disk-mb = 100
maintenance-start-utc = 10:00:00
inbound-http = [
    ["johndoe.com", 8080],
    ["www.johndoe.com", 8080],
]
inbound-tls = [
    ["johndoe.com", 443, 8443], # HTTPS
    ["www.johndoe.com", 443, 8443], # HTTPS
    ["mail.johndoe.com", 465, 8465], # SMTP
]
inbound-dns = [
    ["johndoe.com", 8053, 8053],
]
inbound-ssh = [
    ["ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCmuC02GkkkvQLl87fPdqEmzzyCPSFqauUkWCR1NJ0YuGM6qYT8+Dh0dhlAoJr5UWuBUmxuS9vjHTiWrP0d4/R8mbrfMHOyWrUlssjMw5UR+uvbUo/QN54p5y5nPQ3sooQpD7RcyRwPURBeA9p1oAfIddfWrEUZ/f8A7TKP9i+0fKNGEJrGH3N9CsLOXQDMP3K8UJfWaG6vTWtMoasvSD+g2+BVz6mlbiOg+kYGhrrsBcWdJ8hv4vcTKj99nvgGrzbUh2Gww0hSh7j1mBnHk2DcLF922JjnyO/uErQdpgsMqucQrSzhYPdoyhqJnBRx09GyVuCvKkdNEfbjT0XVvy0FgZsVw9AMnwMblGj6+C3hgtZLIZYEVD20gwkHbb8udI5yr9hcTaXH5knIgbaWZjCnMwv/sTOR+gQbl9llk1Taco7EAxcpOKz+tKARKOl0l9NwkdULuSvIuykgEhFe3zi/P/9mLz4J/DH++EDGSUFcz2jvDAPpRcHmX5fasDyOZTk= user@laptop", 8022],
]
outbound-tcp = [
    ["api.buypass.com", "tcp", 443],
    ["api.example-email-service.com", 443],
]

[legal]
terms = "https://api.example.com/terms-19700101"
i-agree = true
```

- `cores` is the number of CPU cores allocated to the server.
- `ram-mb` is the number of MiB of RAM available to the server.
  When your server hits this limit, the kernel will get OOM errors.
- `disk-mb` is the size in MiB of the persistent block storage device available to the server.
  This may be zero.
- `maintenance-start-utc` is a time of day in UTC.
  When the hosting company needs to slow or shut down the server for scheduled maintenance,
  its staff will try to schedule this maintenance in the hour or two after `maintenance-start-utc`.
- `inbound-http` is a list of `[hostname, local_port]` values.
  When the load balancer receives HTTP requests with a `Location` header matching `hostname`,
  it forwards the request to `local_port` on this server.
- `inbound-tls` is a list of `[name, external_port, local_port]` values.
  When the load balancer receives a TLS connection on `external_port` with server name matching `name`,
  it forwards the TCP connection to `local_port`.
  The server listening on `local_port` must perform TLS protocol.
  It needs a certificate matching `name`.
- `inbound-dns` is a list of `[name, local_udp_port, local_tcp_port]` values.
  When the load balancer receives a UDP request matching `name`,
  it forwards the request to `local_udp_port` or `local_tcp_port`.
- `inbound-ssh-public-keys` is a list of `[pub_key, local_port]` values.
  When the load balancer receives an SSH connection and the client offers `pub_key`,
  it forwards the request to `local_port`.
- `outbound-tcp` is a list of `[hostname, port]` values.
  When the server performs a DNS lookup of `host` and receives A or AAAA records,
  the hosting service adds the IP addresses to the firewall so the server may connect to them on `port`.

## Binary

To start your server or update the binary, PUT the binary file at `/binary?type=TYPE`.

Accepted values for the `type` parameter:
- `x86-64-kvm` means x86 64-bit, KVM hypervisor

Example:
```
$ curl --cert identity.crt -X PUT https://api.example.com/binary?type=x86-64-kvm --upload-file build/server.img
```

To stop the server, DELETE the file at `/binary`.

Example:
```
$ curl --cert identity.crt -X DELETE https://api.example.com/binary
```

## Secrets
You may upload a file with sensitive data.
The API keeps this data encrypted on disk.
The hosting service makes the unencrypted data available
to the server as a read-only block device.

Upload a secrets file, PUT the file at `/secrets`:
```
$ curl --cert identity.crt -X PUT https://api.example.com/secrets --upload-file secrets.env
```

You can also DELETE `/secrets`.

They API cannot retrieve uploaded secrets.

## Backup & Restore

You may download a snapshot of the persistent block storage device with GET `/data`.
Add the `Accept-Encoding: gzip` header to download compressed data.

You may replace the contents of the server's persistent block storage with PUT `/data`.
If you upload compressed data, add the `Content-Encoding: gzip` header.
When the upload completes,
the API stops the server,
replaces the persistent block storage with the uploaded file,
and starts the server.

The API may limit the rate of operations on `/data` and return
[429 Too Many Requests](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/429).
The client should wait and retry the request.

## Metrics
The server must emit [JSON lines](https://jsonlines.org/).
The API collects these lines and makes them available at `/jsonl`.

Each line should be a JSON object with a `ts` field.
The `ts` field is the number of micro-seconds since the epoch, 1970-01-01T00:00:00Z.

The API checks each line.
- If the line is not a JSON object, it wraps it in an error line.  Example:
  ```json
  {"ts":1634609464532729,"error":"bind() failed: Permission denied (13)"}
  ```
- If the line is a JSON object but the `ts` field is missing,
  is not a number,
  is earlier than 1 minute ago,
  or is in the future,
  then the API replaces the line with an error line.  Example:
  ```json
  {"ts":1634609464532729,"error":"metric has invalid `ts` value","original":"{\"ts\":1634609464532,\"info\":\"Hello\"}"}
  ```

Download metrics lines with GET on `/jsonl`.
The records will all have `ts` fields.
The records may be out of order.
But no record will be more than 1 minute earlier than any record before it.

Example:
```
$ curl --cert identity.crt -X GET https://api.example.com/jsonl
```

To get metrics newer than a particular time,
add the [Range](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Range)
header with `ts` for the unit.  Example:
```
$ curl --cert identity.crt -X GET --header 'Range: ts=1634609464000000-' https://api.example.com/jsonl
```

To get some metrics and then stream new metrics as they become available, use STREAM.
Example:
```
$ curl --cert identity.crt -X STREAM --header 'Range: ts=1634609464000000-' https://api.example.com/jsonl
```

The API generates its own metrics and adds them to `/jsonl`:
- `account-balance` - Added when the account balance changes.  Example:
  ```json
  {"ts":1634609464532731,"currency":"usd","account-balance":19.99}
  ```
- `action-required` - Added repeatedly while the account has a problem requiring human action.
  Your monitoring system should create a ticket for an engineer to review during business hours.
  
  When the action is no longer needed, `"action-required": null` appears once.
  
  Examples:
  ```json lines
  {"ts":1634609464532731,"action-required":"Agree to our new terms of use https://api.example.com/terms-20211019"}
  {"ts":1634609464532731,"action-required":null}
  ```
- `working-on-it` - An explanation of degraded service.
  Display this prominently on your monitoring dashboard.

  Examples:
  ```json lines
  {"ts":1634609464532731,"working-on-it":"The machine failed. We are migrating server to a new machine.","human":false,"url":"https://www.example.com/docs/auto-migration.html"}
  {"ts":1634609464532731,"working-on-it":"Our primary network provider is experiencing an outage.","human":true,"url":"https://example-status.com/2021-10-19/"}
  ```
- `health` - boolean value indicating that the service and host are currently healthy.
  When this is false, the API or hosting company stuff should also add
  `working-on-it` records or `action-required` records
  to inform their customers about the problem.
  
  Display this prominently on your monitoring dashboard.
  
  Example:
  ```json
  {"ts":1634609464532731,"health":true}
  ```

## Recover an Account
If you lose your certificate or key,
you must contact the company's support staff to help you recover the account.
They will instruct you to create a new certificate, create a new account, 
add money to pay the recovery fee, and then perform verification steps.

The support staff will add an alert to the server's metrics.

The process takes at least 24 hours.

Once you pass their verification steps,
they will move your new certificate to your old account.

# TO DO
- Remove target port numbers and just use reasonable defaults.
- Figure out how a KVM guest can send a stream to the host.
- Find out if one really can load-balance SSH connections.
- Document KVM block device names for binary, params, secrets, and data.
- Find out limits of device-mapper for snapshots.
- Look into alternatives to device-mapper.
- Decide whether to eliminate UDP DNS forwarding and have proxy convert between UDP & TCP.
- Add public key for downloading metrics
- Add public key for updating binary & secrets
- Add backup public key for downloading snapshots
