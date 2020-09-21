# [SigScale](http://www.sigscale.org) Online Charging System (OCS)

Prebuilt packages available for [Ubuntu](https://raw.githubusercontent.com/sigscale/ocs/master/README.ubuntu)
and [Docker](https://raw.githubusercontent.com/sigscale/ocs/master/README.docker).

[Open Issues](https://sigscale.atlassian.net/projects/OCS/issues/?filter=allopenissues "Open Issues")  
[Create Issue](https://sigscale.atlassian.net/secure/CreateIssue!default.jspa?pid=10100&issuetype=10000 "Create
 Issue")

This application implements functions used by communications
service providers (CSP) for authorization and charging of
prepaid services. It is built to TM Forum standards with
Open APIs for management of product, service and balance. A
web components front end is also provided for standalone use.

## AAA
Authentication, authorization and accounting (AAA) functions
are the foundation to commercial operations of a CSP. Subscriber
credentials may (optionally) be stored internally with
authentication performed over DIAMETER/RADIUS using EAP methods
(AKA/AKA', PWD, TTLS) or managed by an external AAA (e.g. 3GPP HSS).

## OCS
An Online Charging System (OCS) performs real-time charging for
services. An OCS authorizes subscribers' sessions subject to
available credit on account and decrements account balance as
services are consumed.  When a subscriber's account balance is
depleted authorization may be withdrawn and ongoing session(s)
terminated.

## PCRF
A Policy Control and Charging Rules Function (PCRF) encompasses
policy control decision and flow based charging control functionalities. 
The PCRF provides network control regarding the service data flow
detection, gating, QoS and flow based charging,

## 3GPP
This application conforms to 3GPP specifications for the interfaces,
protocols and procedures of the OCS, PCRF, HSS and 3GPP AAA Server
functions in the reference architecture.

## Interfaces
|Interface | Description                      |
|----------|----------------------------------|
|GUI       | Polymer Web Components           |
|REST      | TM Forum Open APIs               |
|CLI       | Erlang API                       |
|RADIUS    | AAA NAS Clients                  |
|DIAMETER  | 3GPP Ro/Gy/Wo,Gx,SWm/STa,SWx,S6a |
|SNMP      | Performance Management           |
|EAP-PWD   | Android, Linux                   |
|EAP-TTLS  | Android, Linux, Apple, Windows   |
|EAP-AKA   | Android, Linux, Apple            |
|EAP-AKA'  | Android, Linux                   |
|IPDR      | Billing Record Files             |

### Graphical User Interface (GUI)
A web front end built with Google [Polymer](https://www.polymer-project.org)
web components for
[material design](https://material.io/guidelines/material-design/introduction.html) 
provides simple guided management of Product Offerings & Prices, Subscribers,
Balance Buckets and NAS clients. Provisioning common authorization attributes
as well as viewing usage and access logs is supported. Uses REST APIs exclusively.
![screenshot](https://raw.githubusercontent.com/sigscale/ocs/master/doc/ocs-gui.png)

### Application Programming Interfaces (API)
The GUI provides a comfortable interface for administration however
most CSPs shall want to integrate Operations & Business Support Systems
(OSS/BSS) using machine-to-machine APIs.

#### [REST](https://en.wikipedia.org/wiki/Representational_state_transfer)
Most aspects of provisioning and operations may be performed through
integration using an HTTP RESTful interface. Specifically the
[TM Forum](https://www.tmforum.org)
[Open APIs](https://www.tmforum.org/open-apis/) are supported including:
Product Catalog, Product Inventory, Prepay Balance, Service Inventory,
Resource Inventory and Usage Management.

#### [Erlang](http://www.erlang.org)
All aspects of provisioning, operations and maintenance may be performed
using the Erlang public API, either manually on the command line
[shell](http://erlang.org/doc/man/shell.html), or through custom Erlang
module development.

### [DIAMETER](http://tools.ietf.org/html/rfc6733)
SigScale OCS supports the DIAMETER applications for the 3GPP interafces
of an OCS (Ro/Gy/Wo) (3GPP 32.299), PCRF (Gx), HSS (S6a) and AAA Server
(STa/SWm/SWx), The OCS function supports Session Charging with
Unit Reservation (SCUR) and Event Charging with Unit Reservation (ECUR)
in CS, PS and IMS domains with both centralized and distributed unit 
determination. Non-3GPP access is supported with for ePDG with either
internal HSS or proxy over SWm to external HSS.

### [RADIUS](http://tools.ietf.org/html/rfc2865)
The OCS acts as an authentication, authorization and accounting (AAA) server
for network access servers (NAS) using the RADIUS protocol such as wireless
local area network (WLAN) access points (AP), broadband remote access server
(BRAS) or broadband network gateway (BNG).

### [SNMP](http://tools.ietf.org/html/rfc3410)
A Simple Network Management Protocol (SNMP) agent is included which allows
a Network Management System (NMS) to interogate the Management Information
Bases (MIB) supported including RADIUS and DIAMETER MIBs.

#### Authentication & Authorization
A NAS may request authentication from the AAA server for subscribers
attempting access. The OCS may authorize access and provide specific
service authorization information (i.e. data rate, class, session expiry
time). In a 3GPP context an external Home Subscriber Server (HSS) may
provide AAA with or without proxy through SigScale AAA.

#### Accounting
A NAS may send accounting requests to the OCS (AAA server) at the end of
a session and optionally at intervals during an ongoing session. The OCS
logs usage records for offline billing and reporting and performs real-time
credit management, updating subscriber account balances.  The OCS may send
a disconnect request to a NAS when an interim update depletes all available
balance or when a subscriber has been disabled administratively.

### [EAP](https://tools.ietf.org/html/rfc3748)
The Extensible Authentication Protocol (EAP) is an authentication framework
which supports multiple authentication methods. In a WLAN (Wi-Fi) use case
an EAP peer (supplicant) in a device (e.g. laptop or smartphone) sends
EAP over LAN (EAPoL) to the AP (NAS) which tunnels the EAP over RADIUS to
the OCS (AAA server). An EAP authentication method (e.g. AKA', PWD, TTLS)
is negotiated and the peer authenticates directly with the OCS.

#### [EAP-AKA/AKA'](https://tools.ietf.org/html/rfc5448)
The AKA/AKA' methods authenticate using the credentials (K/OPc) stored
on the USIM of a mobile device providing mobile operators the same level
of security on non-3GPP access (e.f. Wifi) as 3GPP radio access networks.

#### [EAP-PWD](https://tools.ietf.org/html/rfc5931)
The PWD method authenticates using only a username and a password. This
method addresses the problem of password-based authenticated key exchange
using a (possibly weak) password for authentication to derive an
authenticated and cryptographically strong shared secret. The implementation
in OCS uses Elliptic Curve Cryptography (ECC).

#### [EAP-TTLS](https://tools.ietf.org/html/rfc5281)
The TTLS method uses Transport Layer Security
([TLS](https://tools.ietf.org/html/rfc4346)) protocol that provides for
client authentication of a server, as well as secure ciphersuite
negotiation and key exchange. The secure connection may then be used to
allow the server to authenticate the client using existing, widely deployed
methods such as PAP which is used in OCS.

### [IPDR](https://www.tmforum.org/ipdr)
The Internet Protocol (IP) Detail Record (IPDR) is an industry standard
exchange format for usage records within the Internet Service Provider (ISP)
ecosystem. OCS generates IPDR format usage logs which may be transfered with
SFTP/SCP for offline processing.

