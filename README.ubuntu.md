# Don't knock yourself out! Production ready debian packages are available.

## [Video](https://youtu.be/oL6zyGoxV70)

## Install SigScale package repository configuration:

### Ubuntu 26.04 LTS (resolute)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/ubuntu-resolute/sigscale-release_1.4.8-1+ubuntu26.04_all_5043a960dd58c31f17a2bf82fec705b8.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

### Ubuntu 24.04 LTS (noble)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/ubuntu-noble/sigscale-release_1.4.8-1+ubuntu24.04_all_755effbd9d7e97f0a0e6d012d55e4f0b.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

## Install SigScale OCS:
	sudo apt install ocs
	sudo systemctl enable ocs
	sudo systemctl start ocs
	sudo systemctl status ocs

## Support
Contact <support@sigscale.com> for further assistance.

