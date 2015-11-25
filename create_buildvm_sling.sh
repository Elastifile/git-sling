# clone git-sling repo

mkdir ~/git
cd ~/git
git clone git@github.com:Elastifile/git-sling.git

#configure cron

crontab -l | { cat; echo "* * * * * ssh-agent ~/git/git-sling/sling-build-cron.sh"; } | crontab -

#configure msmtp - create a file /opt/msmtp.conf with permissions for 'build' user
sudo yum install msmtp
sudo cat >> /opt/msmtp.conf <<EOF
defaults
tls on
tls_starttls on
tls_trust_file /etc/pki/tls/certs/ca-bundle.crt
 
account default
host smtp.gmail.com
port 587
auth on
user elasti-prepush@elastifile.com
password ...
from elasti-prepush@elastifile.com
logfile /dev/null
EOF

sudo chown build:rnd /opt/msmtp.conf
sudo chmod 600 /opt/msmtp.conf

# configure tmpfs
echo "tmpfs /mnt/tmpfs tmpfs defaults,noatime,nosuid,nodev,mode=1777,size=4G 0" >> /etc/fstab
