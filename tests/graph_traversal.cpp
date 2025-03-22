#include "common.h"

static suite<"graph traversal efficiency"> _ = [] {
  "graph_traversal_efficiency_basics"_test = [] {
    atom a = 42;
    a();
    expect(a.state->is_up_to_date_counter == 0_i)
        << "accessing a should directly return the value, without checking "
           "if "
           "it's up to date (because it always is)";

    calc b = [=] { return a(); };
    b();
    expect(a.state->is_up_to_date_counter == 0_i)
        << "b is initially not calculated yet, so we know it is not up to "
           "date and don't need to check a whether it is up to date";
  };

  "graph_traversal_efficiency_lazy"_test = [] {
    atom a = 42;
    calc b = [=] { return a(); };

    calc c = [=] { return b(); };
    calc d = [=] { return b(); };
    calc e = [=] {
      c();
      d();
      return 0;
    };

    a.state->is_up_to_date_counter = 0;
    e();
    expect(a.state->is_up_to_date_counter == 0_i)
        << "b is initially not calculated yet, so we know it is not up to "
           "date and don't need to check a whether it is up to date";

    e();
    expect(a.state->is_up_to_date_counter == 1_i)
        << "b should only check a once for determining whether it is up to "
           "date";
  };

  skip / "graph_traversal_efficiency_reactive_bottom_up"_test = [] {
    atom a = 42;
    calc b = [=] { return a(); };

    atom f = true;
    calc g = [=] { return f() ? b() : 0; };
    calc h = [=] { return f() ? 0 : b(); };

    // Note: this autorun is only to ensure that g and h are observed.
    //   We don't really care whether the autorun itself gets
    //   re-evaluated or not. So, if g or h after being
    //   re-evaluated did not change, it's not a problem that this
    //   won't propagate upwards.
    autorun([=] {
      g();
      h();
    });

    // TODO: two tests:
    // 2. making b unreactive wrt g, does that require a full is_up_to_date
    // traversal for b through h?

    // Here we are testing what happens if g and h are being observed (through
    // autorun), but initially only g observes b. After changing f to false,
    // depending on the order of registration, b might no longer become
    // observed, and therefore not reactive anymore. Because of that, if h is
    // evaluated after g, it will start depending on b, but cannot immediately
    // determine it is up to date, due to it not being reactive anymore. We
    // actually want to make sure that b is still up-to-date, even if at some
    // intermediate state, it is not being observed.
    a.state->is_up_to_date_counter = 0;
    f = false;
    expect(a.state->is_up_to_date_counter == 0_i)
        << "b should not need to check a for determining whether it is up to "
           "date, because it was reactive after all";
  };

  skip / "graph_traversal_efficiency_reactive_bottom_up_top_down"_test = [] {
    atom a = 42;
    calc b = [=] { return a(); };

    atom f = true;
    calc g = [=] { return f() ? b(), true : false; };

    autorun([=] {
      if (!g())
        b();
    });

    a.state->is_up_to_date_counter = 0;
    f = false;
    expect(a.state->is_up_to_date_counter == 0_i)
        << "b should not need to check a for determining whether it is up "
           "to date, because it was reactive after all";
  };

  // This unit test makes sure that calls to unobserve() don't trigger
  // unnecessary reactive state propagation (through observe()).
  "unobserve_efficiency"_test = [] {
    atom a = 42;

    calc b = [=] { return a(); };

    {
      auto autorun_1 = autorun([=] { b(); }, nullptr);

      {
        auto autorun_2 = autorun([=] { b(); }, nullptr);

        {
          calc c = [=] { return b(); };
          c();

          a.state->observe_counter = 0;
        }

        expect(a.state->observe_counter == 0_i)
            << "[true, true, false] => [true, true] => no propagation";

        a.state->observe_counter = 0;
      }

      expect(a.state->observe_counter == 0_i)
          << "[true, true] => [true] => no propagation";
    }

    expect(a.state->observe_counter == 1_i) << "[true] => [] => propagation";

    {
      calc c = [=] { return b(); };
      c();

      {
        auto autorun_1 = autorun([=] { b(); }, nullptr);
        a.state->observe_counter = 0;
      }

      expect(a.state->observe_counter == 1_i)
          << "[false, true] => [false] => propagation";

      a.state->observe_counter = 0;
    }

    expect(a.state->observe_counter == 0_i)
        << "[false] => [] => no propagation";
  };
};

