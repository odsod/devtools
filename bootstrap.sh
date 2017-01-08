#!/usr/bin/env bash

set -eu

msg() {
  echo
  echo '****************************************'
  echo "${1} ..."
  echo '****************************************'
}

msg 'Collecting variables'
read -rp 'Hostname: ' hostname
read -rp 'Username: ' username
read -rp 'Full name: ' full_name
read -rp 'Email: ' email

msg 'Creating single bootable /dev/sda1 partition'
sfdisk /dev/sda <<< '2048,,L,*'
mkfs.ext4 /dev/sda1

msg 'Mounting /dev/sda1 to /mnt'
mount /dev/sda1 /mnt

msg 'Writing /etc/fstab'
genfstab -U /mnt >> /mnt/etc/fstab

msg 'Installing moreutils'
pacman --noconfirm -Sy moreutils

msg 'Configuring Swedish Pacman mirrors'
cat \
  <(grep --no-group-separator -A1 Sweden /etc/pacman.d/mirrorlist) \
  <(grep -v --no-group-separator -A1 Sweden /etc/pacman.d/mirrorlist) \
  | sponge /etc/pacman.d/mirrorlist

msg 'Installing Arch Linux to /mnt'
pacstrap /mnt base base-devel

msg 'Setting keyboard layout to dvorak'
echo 'KEYMAP=dvorak' >> /mnt/etc/vconsole.conf

msg 'Setting console font'
echo 'FONT=Lat2-Terminus16' >> /mnt/etc/vconsole.conf

msg 'Setting time zone'
arch-chroot /mnt \
  ln -s /usr/share/zoneinfo/Europe/Stockholm /etc/localtime

msg 'Configuring locale'
sed -i 's/#en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /mnt/etc/locale.gen
echo 'LANG=en_US.UTF-8' >> /mnt/etc/locale.conf
arch-chroot /mnt \
  locale-gen

msg 'Installing grub'
arch-chroot /mnt \
  pacman --noconfirm -S grub
arch-chroot /mnt \
  grub-install --target=i386-pc /dev/sda
arch-chroot /mnt \
  grub-mkconfig -o /boot/grub/grub.cfg

msg 'Generating ramdisk'
arch-chroot /mnt \
  mkinitcpio -p linux

msg 'Enabling dhcpcd service'
arch-chroot /mnt \
  systemctl enable dhcpcd.service

msg 'Creating user'
arch-chroot /mnt \
  useradd -m -g users -s /bin/bash "$username"
echo "${username} ALL=(ALL) NOPASSWD: ALL" >> /mnt/etc/sudoers

msg 'Installing packages required for bootstrapping'
arch-chroot /mnt \
  pacman --noconfirm -S grub git ansible

msg 'Cloning pbox'
arch-chroot /mnt \
  sudo -u poscar \
    git clone https://github.com/odsod/pbox /home/"$username"/pbox
sed -i 's#https://github.com/#git@github.com:' /mnt/home/"$username"/pbox/.git/config

msg 'Writing pbox variables'
cat > /mnt/home/"$username"/pbox/vars.json <<EOF
{
  "name": "${full_name}",
  "email": "${email}",
  "host": "${hostname}"
}
EOF
arch-chroot /mnt \
  chown "$username":users /home/"$username"/pbox/vars.json

msg 'Bootstrapping pbox'
arch-chroot /mnt \
  sudo -u poscar \
    /home/"$username"/pbox/files/scripts/pbox

msg 'Setting root password'
arch-chroot /mnt \
  passwd
msg 'Setting user password'
arch-chroot /mnt \
  passwd "$username"

msg 'Done!'
