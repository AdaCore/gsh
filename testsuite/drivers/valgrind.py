from e3.testsuite.process import check_call


def check_call_valgrind(driver, cmd, test_name=None, result=None, **kwargs):
    """
    Wrapper for `e3.testsuite.process` that runs the process under Valgrind if
    this is a Valgrind-checked testsuite run. The process exit status will be
    2 if Valgrind finds memory issues.
    """
    if driver.env.valgrind:
        cmd = ['valgrind', '-q', '--error-exitcode=2',
               '--leak-check=full'] + cmd
    return check_call(driver, cmd, test_name, result, **kwargs)
