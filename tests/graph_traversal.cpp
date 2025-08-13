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

  "observing_autorun_efficiency"_test = [] {
    atom a = 42;
    calc b = [=] { return a(); };

    // This autorun is only to ensure that b is observed.
    // We don't really care whether the autorun itself gets
    // re-evaluated or not. So, if b after being
    // re-evaluated did not change, it's not a problem that this
    // won't propagate upwards.
    autorun([=] { b(); });

    a.state->is_up_to_date_counter = 0;
    expect(a.state->is_up_to_date_counter == 0_i)
        << "b should not need to check a for determining whether it is up to "
           "date, because it was reactive after all";
  };

  "is_up_to_date_efficiency_outdated"_test = [] {
    atom a = 42;
    calc b = a;
    calc c = b;

    c();
    expect(a.state->is_up_to_date_hierarchy_traversal_counter == 0_i);
    expect(b.state->is_up_to_date_hierarchy_traversal_counter == 0_i);
    expect(c.state->is_up_to_date_hierarchy_traversal_counter == 0_i);

    a.state->is_up_to_date_hierarchy_traversal_counter = 0;
    b.state->is_up_to_date_hierarchy_traversal_counter = 0;
    c.state->is_up_to_date_hierarchy_traversal_counter = 0;

    c();
    expect(a.state->is_up_to_date_hierarchy_traversal_counter == 0_i);
    expect(b.state->is_up_to_date_hierarchy_traversal_counter == 1_i);
    expect(c.state->is_up_to_date_hierarchy_traversal_counter == 1_i);

    a.state->is_up_to_date_hierarchy_traversal_counter = 0;
    b.state->is_up_to_date_hierarchy_traversal_counter = 0;
    c.state->is_up_to_date_hierarchy_traversal_counter = 0;

    a = 1729;
    c();
    expect(a.state->is_up_to_date_hierarchy_traversal_counter == 0_i);
    expect(b.state->is_up_to_date_hierarchy_traversal_counter == 0_i);
    expect(c.state->is_up_to_date_hierarchy_traversal_counter == 0_i);
  };

  // TODO: move to functional/correctness tests
  "observing_autorun"_test = [] {
    atom a = 42;
    calc b = [=] {
      a();
      return 0;
    };

    atom c = true;
    autorun([=] {
      if (c)
        b();
    });

    atom e = true;
    auto z = false;
    autorun([=, &z] {
      z = true;
      if (e)
        b();
    });

    // At this point b is observed by both autoruns

    // We disable both autoruns
    c = false;
    e = false;

    // We modify a, which should mark b as maybe_changed (after two notification
    // sweeps, the stale_count is zero, but since a changed the second sweep
    // will mark b as maybe_changed).
    // Note that b is not recalculated yet, because it is not reactive.
    a = 1729;

    // Re-enable the first autorun. This will recalculate b, as it is directly
    // used in the autorun.
    // A side-effect of b being recalculated, is that it will notify
    // all its observers. The first autorun will therefore be notified, but with
    // the message that b did not change (it always returns 0). This means that
    // the stale_count will be decremented, resulting in a negative stale_count
    // of -1. In the end, this turns out not to be an issue, because once the
    // autorun finishes execution, the stale_count will be set to zero again.
    // However, the second autorun, even though it is not reactive, still
    // receives notifications. Its stale_count will therefore also become
    // negative (-1), but in this case it will not be reset to zero, because it
    // wasn't executing. As a result, it's left in an invalid state.
    c = true;

    // Reset z so we can check whether the second autorun did run.
    z = false;

    // Now, by setting e to true, we should be triggering the second autorun.
    // Unfortunately, because the stale_count is incremented
    // during the first sweep and decremented during the second sweep, leaving
    // it at -1, it will not run, because it will only be recalculated if it
    // becomes zero.
    e = true;
    expect(z == true);
  };

  // TODO: add a test that makes sure that we won't evaluate an observer before
  // all observables are guaranteed to be up to date.

  "nonobserving_autorun"_test = [] {
    atom a = true;
    atom b = 42;

    auto z = false;
    autorun([=, &z] {
      z = true;
      if (a)
        b();
    });

    // Disable autorun.
    z = false;
    a = false;
    expect(z == true);

    // Changing b should not trigger the autorun, which is disabled.
    z = false;
    b = 1729;
    expect(z == false);
  };

  "nonobserving_calc"_test = [] {
    atom a = true;
    atom b = 42;

    auto z = false;
    calc c = [=, &z] {
      z = true;
      if (a)
        b();
      return 0;
    };

    c();

    // Make c not observing b
    z = false;
    a = false;
    c();
    expect(z == true);

    // Changing b should not result in c needing recalculation, because it is
    // not observing b.
    z = false;
    b = 0;
    c();
    expect(z == false);
  };
};
