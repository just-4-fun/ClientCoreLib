package just4fun.android.core

import java.util.concurrent.ThreadPoolExecutor
import android.os.{HandlerThread, Looper}

package object async {

	/** Can be replaced for more specific ThreadPoolExecutor. */
	var threadPoolExecutor: ThreadPoolExecutor = _
}