package just4fun.android.core.app

import android.content.pm.PackageManager

trait AppUtils {
	app: App =>


	def hasPermission(prm: String): Boolean = getPackageManager.checkPermission(prm, getPackageName) == PackageManager.PERMISSION_GRANTED


}
