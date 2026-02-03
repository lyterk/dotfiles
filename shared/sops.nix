{
  ...
}:

{
  sops = {
    age.keyFile = "/home/lyterk/.config/sops/age/keys.txt";
    defaultSopsFile = ../secrets/default-secret.yaml;
    secrets = {
      sshPrivateKey = {
        # It breaks when I try to make these paths, which feels silly but oh well.
        path = "/etc/nixos/secrets/ssh-private-key";
        owner = "lyterk";
        group = "users";
        mode = "0600";
      };
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
    };
  };
}
