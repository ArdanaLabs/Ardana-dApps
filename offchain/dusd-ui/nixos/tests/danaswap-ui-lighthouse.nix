{ nixosTest
, dusd-ui # The dusd-ui website
, lighthouse # The lighthouse executable
, categories ? { performance = 0.99; accessibility = 0.99; seo = 0.99; best-practices = 0.99; } # The minimal score that needs to be reached for each category
}:
let
  name = "dusd-ui-lighthouse";
  dusd-ui-server-port = "8000";
in
nixosTest {
  inherit name;

  nodes = {
    server = { config, pkgs, ... }: {
      users.users.dusd-ui = {
        name = "dusd-ui";
        group = "dusd-ui";
        isSystemUser = true;
      };
      users.groups.dusd-ui = { };

      systemd.services.dusd-ui-server = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        description = "dusd-ui-server";
        serviceConfig = {
          User = "dusd-ui";
          Group = "dusd-ui";
          ExecStart = ''${pkgs.simple-http-server}/bin/simple-http-server -c=js,css,svg,html -i -p ${dusd-ui-server-port} -- ${dusd-ui}'';
        };
      };
    };
  };
  testScript = ''
    import json
    import os

    start_all()
    server.wait_for_unit("network.target")
    server.wait_for_open_port(${dusd-ui-server-port})

    report_path = "/tmp/lighthouse-report.json"
    server.succeed("CI=1 ${lighthouse}/bin/lighthouse http://localhost:${dusd-ui-server-port} --output json --output-path {} --only-categories accessibility,best-practices,performance,seo --skip-audits valid-source-maps --chrome-flags=\"--headless --no-sandbox\"".format(report_path))
    server.copy_from_vm(report_path)

    with open("{}/lighthouse-report.json".format(os.environ["out"]), "r") as f:
      report = json.load(f)
      categories = report["categories"]
      assert categories["performance"]["score"]    >= ${toString categories.performance}, "performance score should be at least ${toString (categories.performance * 100)}%"
      assert categories["accessibility"]["score"]  >= ${toString categories.accessibility}, "accessibility score should be at least ${toString (categories.accessibility * 100)}%"
      assert categories["seo"]["score"]            >= ${toString categories.seo}, "seo score should be at least ${toString (categories.seo * 100)}%"
      assert categories["best-practices"]["score"] >= ${toString categories.best-practices}, "best-practices score should be at least ${toString (categories.best-practices * 100)}%"
  '';
}
