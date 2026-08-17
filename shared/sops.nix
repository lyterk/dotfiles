{
  ...
}:
{
  sops = {
    age.keyFile = "/var/lib/sops-nix/keys.txt";
    defaultSopsFile = ../secrets/default-secret.yaml;
    secrets = {
      gpgCode = {
        path = "/etc/nixos/secrets/code-gpg-key";
        owner = "lyterk";
        group = "users";
        mode = "0600";
      };
      gpgKev = {
        path = "/etc/nixos/secrets/kev-gpg-key";
        owner = "lyterk";
        group = "users";
        mode = "0600";
      };
      homeSshPrivateKey = {
        owner = "lyterk";
        group = "users";
        mode = "0600";
        path = "/home/lyterk/.ssh/id_ed25519";
        sopsFile = ../secrets/sshKeys.yaml;
      };
      homeSshPrivateKeyForWork = {
        key = "homeSshPrivateKey";
        owner = "work";
        group = "users";
        mode = "0600";
        path = "/home/work/.ssh/id_ed25519";
        sopsFile = ../secrets/sshKeys.yaml;
      };
      workSshPrivateKey = {
        owner = "work";
        group = "users";
        mode = "0600";
        path = "/home/work/.ssh/work_ed25519";
        sopsFile = ../secrets/sshKeys.yaml;
      };
      resticSecrets = {
        sopsFile = ../secrets/resticSecrets.yaml;
        mode = "0444";
      };
      githubToken = {
        sopsFile = ../secrets/githubToken.yaml;
        mode = "0444";
      };
    };
  };
}
