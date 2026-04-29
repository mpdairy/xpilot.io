# Deploy

Production setup for xpilot.io. Tested on Ubuntu 24.04 LTS / Linode 1GB shared.

## What's here

- `Caddyfile` — Caddy reverse proxy + TLS terminator. Serves the wasm
  client and proxies `/ws` to the local game server. Auto-fetches a
  Let's Encrypt cert.
- `xpilot.service` — systemd unit for the game server. Runs as a non-login
  `xpilot` system user with the standard sandbox directives.

## First-time setup on a fresh Ubuntu 24.04 box

Assumes `root` SSH key access and a domain pointing at the box.

```bash
# Update + base packages
apt update && apt upgrade -y
apt install -y ufw fail2ban unattended-upgrades curl gnupg ca-certificates apt-transport-https debian-keyring debian-archive-keyring

# Caddy from the official repo
curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | tee /etc/apt/sources.list.d/caddy-stable.list
apt update && apt install -y caddy

# System user that owns the binary + maps
useradd --system --create-home --home-dir /opt/xpilot --shell /usr/sbin/nologin xpilot
mkdir -p /opt/xpilot/maps /var/www/xpilot
chown -R xpilot:xpilot /opt/xpilot

# SSH hardening (key-only, no root password login)
cat > /etc/ssh/sshd_config.d/00-xpilot-hardening.conf <<EOF
PasswordAuthentication no
KbdInteractiveAuthentication no
PermitRootLogin prohibit-password
PubkeyAuthentication yes
EOF
sshd -t && systemctl reload ssh

# Firewall — ssh + http(s) + WebRTC ephemeral UDP range
ufw default deny incoming
ufw default allow outgoing
ufw allow 22/tcp
ufw allow 80/tcp
ufw allow 443/tcp
ufw allow 443/udp     # HTTP/3
ufw allow 49152:65535/udp  # WebRTC media
ufw --force enable

systemctl enable --now fail2ban
systemctl enable --now unattended-upgrades

# Drop the deploy files into place
cp Caddyfile /etc/caddy/Caddyfile
cp xpilot.service /etc/systemd/system/xpilot.service
systemctl daemon-reload
systemctl reload caddy
```

## Build + deploy from a dev box

Run from the repo root:

```bash
# Build server (release) and client (release wasm)
cargo build --release -p server
( cd client && trunk build --release )

# Push to linode (set HOST to your box's address)
HOST=root@50.116.41.18
KEY=~/.ssh/id_ed25519

rsync -av --delete -e "ssh -i $KEY" target/release/server $HOST:/opt/xpilot/server
rsync -av --delete -e "ssh -i $KEY" maps/                 $HOST:/opt/xpilot/maps/
rsync -av --delete -e "ssh -i $KEY" client/dist/          $HOST:/var/www/xpilot/

ssh -i $KEY $HOST '
  chown -R xpilot:xpilot /opt/xpilot
  chown -R www-data:www-data /var/www/xpilot
  chmod 755 /var/www /var/www/xpilot
  chmod -R a+r /var/www/xpilot
  systemctl restart xpilot
'
```

Caddy doesn't need a restart for static-file changes — it reads from disk on every request. Restart only if you edit the Caddyfile (`systemctl reload caddy`).

## Operational

- `systemctl status xpilot` — service status
- `systemctl restart xpilot` — restart the game server (auto-restarts on failure too)
- `journalctl -u xpilot -f` — tail game server logs
- `journalctl -u caddy -f` — tail Caddy logs
- `fail2ban-client status sshd` — see who's getting banned
- `ufw status verbose` — firewall rules
