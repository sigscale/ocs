# Don't knock yourself out! Production ready debian packages are available.

## [Video](https://youtu.be/CQg9-azYjeo)

## Install SigScale package repository configuration:

### Debian 12 (bookworm)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/debian-bookworm/sigscale-release_1.4.5-1+debian12_all_dc4f6c6b7f70b2853c71dac983dc4008.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

## Install SigScale OCS:
	sudo apt install ocs
	sudo systemctl enable ocs
	sudo systemctl start ocs
	sudo systemctl status ocs

## Support
Contact <support@sigscale.com> for further assistance.

