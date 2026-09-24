# Don't knock yourself out! Production ready debian packages are available.

## [Video](https://youtu.be/oL6zyGoxV70)

## Install SigScale package repository configuration:

### Ubuntu 26.04 LTS (resolute)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/ubuntu-resolute/sigscale-release_1.4.7-5+ubuntu26.04_all_f7695edbbe769fc8448379721dfa03ff.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

### Ubuntu 24.04 LTS (noble)
	curl -sLO https://asia-east1-apt.pkg.dev/projects/sigscale-release/pool/ubuntu-noble/sigscale-release_1.4.7-2+ubuntu24.04_all_9df9e55b3b3b418648d5854c973b6cad.deb
	sudo dpkg -i sigscale-release_*.deb
	sudo apt update

## Install SigScale OCS:
	sudo apt install ocs
	sudo systemctl enable ocs
	sudo systemctl start ocs
	sudo systemctl status ocs

## Support
Contact <support@sigscale.com> for further assistance.

