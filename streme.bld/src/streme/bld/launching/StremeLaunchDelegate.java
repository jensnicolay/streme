package streme.bld.launching;

import java.io.File;
import java.text.MessageFormat;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.IStatusHandler;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.jdt.internal.launching.LaunchingMessages;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IVMInstall;
import org.eclipse.jdt.launching.IVMRunner;
import org.eclipse.jdt.launching.JavaLaunchDelegate;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jdt.launching.VMRunnerConfiguration;

public class StremeLaunchDelegate extends JavaLaunchDelegate
{

  /* (non-Javadoc)
   * @see org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate#getMainTypeName(org.eclipse.debug.core.ILaunchConfiguration)
   */
  public String getMainTypeName(ILaunchConfiguration configuration) throws CoreException {
      return "streme.lang.Streme3";
  }
  
  @Override
  public void launch(ILaunchConfiguration configuration, String mode,
  		ILaunch launch, IProgressMonitor monitor) throws CoreException
  {
  	IVMInstall vm = JavaRuntime.getDefaultVMInstall();
		IVMRunner runner = vm.getVMRunner(mode);
		if (runner == null) {
			abort(MessageFormat.format(LaunchingMessages.JavaLocalApplicationLaunchConfigurationDelegate_0, new String[]{vm.getName(), mode}), null, IJavaLaunchConfigurationConstants.ERR_VM_RUNNER_DOES_NOT_EXIST); 
		}
		
		String mainTypeName = verifyMainTypeName(configuration);

		File workingDir = verifyWorkingDirectory(configuration);
		String workingDirName = null;
		if (workingDir != null) {
			workingDirName = workingDir.getAbsolutePath();
		}
		
		// Classpath
		String[] classpath = getClasspath(configuration);
		
		// Create VM config
		VMRunnerConfiguration runConfig = new VMRunnerConfiguration(mainTypeName, classpath);
//		runConfig.setProgramArguments(execArgs.getProgramArgumentsArray());
//		runConfig.setEnvironment(envp);
//		runConfig.setVMArguments(execArgs.getVMArgumentsArray());
//		runConfig.setWorkingDirectory(workingDirName);
//		runConfig.setVMSpecificAttributesMap(vmAttributesMap);

		// Bootpath
		runConfig.setBootClassPath(getBootpath(configuration));
		
		// check for cancellation
		if (monitor.isCanceled()) {
			return;
		}		
		
		// stop in main
		prepareStopInMain(configuration);
		
		// done the verification phase
		monitor.worked(1);
		
		monitor.subTask(LaunchingMessages.JavaLocalApplicationLaunchConfigurationDelegate_Creating_source_locator____2); 
		// set the default source locator if required
		setDefaultSourceLocator(launch, configuration);
		monitor.worked(1);		
		
		// Launch the configuration - 1 unit of work
		runner.run(runConfig, launch, monitor);
		
		// check for cancellation
		if (monitor.isCanceled()) {
			return;
		}	
}
  
  /*
	 * 
	 * @see org.eclipse.debug.core.model.ILaunchConfigurationDelegate2#preLaunchCheck(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.core.runtime.IProgressMonitor)
	 */
	public boolean preLaunchCheck(ILaunchConfiguration configuration, String mode, IProgressMonitor monitor) throws CoreException {
		if (!saveBeforeLaunch(configuration, mode, monitor)) {
			return false;
		}
		if (mode.equals(ILaunchManager.RUN_MODE) && configuration.supportsMode(ILaunchManager.DEBUG_MODE)) {
			IBreakpoint[] breakpoints= getBreakpoints(configuration);
           if (breakpoints == null) {
               return true;
           }
			for (int i = 0; i < breakpoints.length; i++) {
				if (breakpoints[i].isEnabled()) {
					IStatusHandler prompter = DebugPlugin.getDefault().getStatusHandler(promptStatus);
					if (prompter != null) {
						boolean launchInDebugModeInstead = ((Boolean)prompter.handleStatus(switchToDebugPromptStatus, configuration)).booleanValue();
						if (launchInDebugModeInstead) { 
							return false; //kill this launch
						} 
					}
					// if no user prompt, or user says to continue (no need to check other breakpoints)
					return true;
				}
			}
		}	
		// no enabled breakpoints... continue launch
		return true;
	}

  
//  @Override
//  public String[] getClasspath(ILaunchConfiguration configuration) throws CoreException
//  {
//    return new String[] {"C:\\dev\\runtime-EclipseApplication\\testj\\bin", "C:\\dev\\ws\\streme.bld\\lib\\streme.jar"};
//  }
    
}
