#!/bin/sh

# Auto-detect the default network interface (the one with the default route)
HOST_IF=$(ip route show default | awk '{print $5; exit}')
if [ -z "$HOST_IF" ]; then
  echo "Error: No default network interface found."
  exit 1
fi
echo "Detected host interface: $HOST_IF"

# Create TAP interface
sudo ip tuntap add tap0 mode tap user $USER
sudo ip addr add 10.0.0.1/24 dev tap0
sudo ip link set tap0 up

# Enable IP forwarding
sudo sysctl -w net.ipv4.ip_forward=1

# Set up NAT/masquerade
sudo iptables -t nat -A POSTROUTING -s 10.0.0.0/24 -o "$HOST_IF" -j MASQUERADE

# Start dnsmasq as DHCP + DNS forwarder on tap0
# Guest will auto-configure via DHCP (IP, gateway, DNS)
if command -v dnsmasq >/dev/null 2>&1; then

  # Ensure FORWARD chain allows our traffic (some distros default to DROP)
  #sudo iptables -I FORWARD -i tap0 -o "$HOST_IF" -j ACCEPT
  #sudo iptables -I FORWARD -i "$HOST_IF" -o tap0 -m state --state RELATED,ESTABLISHED -j ACCEPT

  sudo dnsmasq \
    --interface=tap0 \
    --bind-interfaces \
    --dhcp-range=10.0.0.2,10.0.0.254,255.255.255.0,12h \
    --dhcp-option=option:router,10.0.0.1 \
    --dhcp-option=option:dns-server,10.0.0.1 \
    --pid-file=/var/run/dnsmasq-tap0.pid \
    --log-facility=/var/log/dnsmasq-tap0.log \
    --no-resolv \
    --server=8.8.8.8 \
    --server=8.8.4.4

  echo "dnsmasq started: DHCP + DNS on tap0"
  echo "NAT network set up (internet access via $HOST_IF)."

else

  # Ensure FORWARD chain allows our traffic (some distros default to DROP)
  sudo iptables -A FORWARD -i tap0 -o "$HOST_IF" -j ACCEPT
  sudo iptables -A FORWARD -i "$HOST_IF" -o tap0 -m state --state RELATED,ESTABLISHED -j ACCEPT

  echo "Warning: dnsmasq not found. Install it for automatic guest configuration."
  echo "  Without dnsmasq, configure guest manually:"
  echo "    IP 10.0.0.2/24, Gateway 10.0.0.1, DNS 8.8.8.8"

  echo "NAT network set up."
  echo "Guest should use: IP 10.0.0.2/24, Gateway 10.0.0.1, DNS 8.8.8.8"

fi
