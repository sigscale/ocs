# Don't knock yourself out! Production ready RPM packages are available.

## Install SigScale package repository configuration:

### RHEL 9 (plow)
	curl -sLO https://asia-east1-yum.pkg.dev/projects/sigscale-release/rhel-9/Packages/b50798744ef8a50aba206d6308fcc7aad2e99894da5d62aef7a81dd0d1c06283-sigscale-release-1.0.0-1.noarch.rpm
	sudo rpm --install *-sigscale-release-1.0.0-1.noarch.rpm
	sudo dnf makecache

### RHEL 8 (ootpa)
	curl -sLO https://asia-east1-yum.pkg.dev/projects/sigscale-release/rhel-8/Packages/8ca76582b3ce3eb5ae0728e466123e59076244e0e3e5187d0983109dd4c37cae-sigscale-release-1.0.0-1.noarch.rpm
	sudo rpm --install *-sigscale-release-1.0.0-1.noarch.rpm
	sudo dnf makecache

## Install SigScale OCS:
	sudo dnf install sigscale-ocs
	sudo systemctl enable ocs
	sudo systemctl start ocs
	sudo systemctl status ocs

## Support
Contact <support@sigscale.com> for further assistance.

