# -*- mode: ruby -*-
# vi: set ft=ruby :

MACHINES =
  { "ubuntu1204-i386" =>
      { :box       => "chef/ubuntu-12.04-i386",
        :provision => "utils/vagrant/bootstrap-deb.sh"
      },
    "ubuntu1204-amd64" =>
      { :box       => "chef/ubuntu-12.04",
        :provision => "utils/vagrant/bootstrap-deb.sh"
      },
    "centos65-i386" =>
      { :box       => "chef/centos-6.5-i386",
        :provision => "utils/vagrant/bootstrap-rhel.sh"
      },
    "centos65-amd64" =>
      { :box       => "chef/centos-6.5",
        :provision => "utils/vagrant/bootstrap-rhel.sh"
      },
    "debian74-i386" =>
      { :box       => "chef/debian-7.4-i386",
        :provision => "utils/vagrant/bootstrap-deb.sh"
      },
    "debian74-amd64" =>
      { :box       => "chef/debian-7.4",
        :provision => "utils/vagrant/bootstrap-deb.sh"
      }
  }

VAGRANTFILE_API_VERSION = "2"
Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  MACHINES.each_pair do |name, opts|
    config.vm.define name do |c|
      c.vm.box = opts[:box]
      c.vm.network "public_network"
      c.vm.provision :shell, :path => opts[:provision]
      c.vm.provider "virtualbox" do |vb|
        vb.gui = false; vb.memory = 4096; vb.cpus = 2
        vb.customize ["modifyvm", :id, "--natdnshostresolver1", "on"]
      end
      c.vm.provider "vmware_workstation" do |vb|
        vb.gui = false; vb.vmx["memsize"]  = "4096"; vb.vmx["numvcpus"] = "2"
      end
      c.vm.provider "vmware_fusion" do |vb|
        vb.gui = false; vb.vmx["memsize"]  = "4096"; vb.vmx["numvcpus"] = "2"
      end
    end
  end
end
