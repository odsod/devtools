#!/usr/bin/env bash

set -eu

pacman --noconfirm -Sy cowsay moreutils

cowsay 'Collecting variables'
read -rp 'Hostname: ' hostname
read -rp 'Username: ' username
read -rp 'VirtualBox Guest?: ' is_virtualbox_guest

cowsay 'Creating single bootable /dev/sda1 partition'
sfdisk /dev/sda <<< '2048,,L,*'
mkfs.ext4 /dev/sda1

cowsay 'Mounting /dev/sda1 to /mnt'
mount /dev/sda1 /mnt

cowsay 'Configuring Swedish Pacman mirrors'
cat \
  <(grep --no-group-separator -A1 Sweden /etc/pacman.d/mirrorlist) \
  <(grep -v --no-group-separator -A1 Sweden /etc/pacman.d/mirrorlist) \
  | sponge /etc/pacman.d/mirrorlist

cowsay 'Installing Arch Linux to /mnt'
pacstrap /mnt base base-devel

cowsay 'Writing /etc/fstab'
genfstab -U /mnt >> /mnt/etc/fstab

cowsay 'Setting keyboard layout to dvorak'
echo 'KEYMAP=dvorak' >> /mnt/etc/vconsole.conf

cowsay 'Setting console font'
echo 'FONT=Lat2-Terminus16' >> /mnt/etc/vconsole.conf

cowsay 'Setting time zone'
arch-chroot /mnt \
  ln -fs /usr/share/zoneinfo/Europe/Stockholm /etc/localtime

cowsay 'Configuring locale'
sed -i 's/#en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /mnt/etc/locale.gen
echo 'LANG=en_US.UTF-8' >> /mnt/etc/locale.conf
arch-chroot /mnt \
  locale-gen

cowsay 'Installing grub'
arch-chroot /mnt \
  pacman --noconfirm -S grub
arch-chroot /mnt \
  grub-install --target=i386-pc /dev/sda
arch-chroot /mnt \
  grub-mkconfig -o /boot/grub/grub.cfg

cowsay 'Generating ramdisk'
arch-chroot /mnt \
  mkinitcpio -p linux

cowsay 'Enabling dhcpcd service'
arch-chroot /mnt \
  systemctl enable dhcpcd.service

cowsay 'Creating user'
arch-chroot /mnt \
  useradd -m -g users -s /bin/bash "$username"
echo "${username} ALL=(ALL) NOPASSWD: ALL" >> /mnt/etc/sudoers

cowsay 'Installing packages required for bootstrapping'
arch-chroot /mnt \
  pacman --noconfirm -S grub git ansible

cowsay 'Cloning odbox'
arch-chroot /mnt \
  sudo -u "$username" \
    git clone https://github.com/odsod/odbox /home/"$username"/odbox
sed -i 's#https://github.com/#git@github.com:#' /mnt/home/"$username"/odbox/.git/config

cowsay 'Writing odbox variables'
cat > /mnt/home/"$username"/odbox/vars.yml <<EOF
username: ${username}
host: ${hostname}
is_virtualbox_guest: ${is_virtualbox_guest}
EOF
arch-chroot /mnt \
  chown "$username":users /home/"$username"/odbox/vars.yml

cowsay 'Bootstrapping odbox'
arch-chroot /mnt \
  sudo -u "$username" \
    /home/"$username"/odbox/files/scripts/odbox

cowsay 'Setting root password'
arch-chroot /mnt \
  passwd
cowsay 'Setting user password'
arch-chroot /mnt \
  passwd "$username"

cowsay 'All done!'
