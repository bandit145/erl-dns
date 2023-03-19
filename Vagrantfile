Vagrant.configure("2") do |config|

        config.vm.define "client" do |client|
        	client.vm.provider "virtualbox" do |vm|
        		vm.memory =  2048
        		vm.cpus = 1
        	end
                client.vm.box = "bento/centos-stream-9"
                client.vm.network "private_network", ip: "192.168.50.2", auto_config: false
                client.vm.provision "shell", inline: "dnf install -y epel-release dhclient"
        end
        config.vm.define "server" do |client|
                client.vm.provider "virtualbox" do |vm|
                        vm.memory =  2048
                        vm.cpus = 1
                end
                client.vm.box = "bento/centos-stream-9"
                client.vm.network "private_network", ip: "192.168.50.3", auto_config: false
                client.vm.provision "shell", inline: "dnf install -y epel-release"
                client.vm.provision "shell", inline: "dnf install -y kea"
                client.vm.provision "file", source: "config_files/kea-dhcp4.conf", destination: "/tmp/kea-dhcp4.conf"
                client.vm.provision "file", source: "config_files/kea-dhcp6.conf", destination: "/tmp/kea-dhcp6.conf"
                client.vm.provision "shell", inline: "cp /tmp/*.conf /etc/kea/"
                client.vm.provision "shell", inline: "systemctl start kea-dhcp4 && systemctl start kea-dhcp6"
        end

end