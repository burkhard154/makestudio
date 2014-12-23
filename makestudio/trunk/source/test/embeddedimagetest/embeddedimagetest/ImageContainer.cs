using System;
using System.Windows.Forms;
using System.Collections;
using System.Drawing;
using System.Diagnostics;

namespace EmbeddedImageTest{
	public enum EPlugInImage {
		Open,
		Save,
		Undo,
		Wait,

		DeviceConfig,
		CardConfig,
		UserAdministration,
		Monitor,
		OffsetAdjustment,
		AppSettings,

		Reset,
		DeviceSelection,
		OnlineFamos,

		DeleteUser,

		Folder,
		Folders,
		TestCase,
		TestCaseSelected,

		Connect,
		Disconnect,
		Prepare,
		Start,
		Start2,
		Stop,

		Channel,

		CharacteristicCurve,
		CharacteristicCurveSelected,
		DeletePoint,
	}

	public class CPlugInImages {
		public CPlugInImages() {
			m_htImgIndicies = new Hashtable();
			m_cImageList = new ImageList();
			m_cImageList.ColorDepth = ColorDepth.Depth32Bit;
			m_cImageList.TransparentColor = Color.Fuchsia;
			foreach (EPlugInImage eImage in Enum.GetValues(typeof(EPlugInImage))) {
				Bitmap cBitmap = null;
				try {
					cBitmap = new Bitmap(this.GetType(), GetFileName(eImage));
				}
				catch {
					// ignorieren
				}
				if (null != cBitmap) {
					int iImageIndex = m_cImageList.Images.Add(cBitmap, Color.Fuchsia);
					m_htImgIndicies.Add(eImage, iImageIndex);
				}
			}
		}

		private string GetFileName(EPlugInImage eImage) {
			switch (eImage) {
				case EPlugInImage.Open :				return "imgs.open.bmp";
				case EPlugInImage.Save :				return "imgs.save_green16.bmp";
				case EPlugInImage.Undo :				return "imgs.undo.bmp";
				case EPlugInImage.Wait :				return "imgs.wait.bmp";
				
				case EPlugInImage.DeviceConfig :		return "imgs.deviceconfig.bmp";
				case EPlugInImage.CardConfig :			return "imgs.card.bmp";
				case EPlugInImage.UserAdministration :	return "imgs.group_user16.bmp";
				case EPlugInImage.Monitor :				return "imgs.monitor2.bmp";
				case EPlugInImage.OffsetAdjustment :	return "imgs.adjust.bmp";
				case EPlugInImage.AppSettings :			return "imgs.preferences16.bmp";

				case EPlugInImage.Reset :				return "imgs.reset.bmp";
				case EPlugInImage.DeviceSelection :		return "imgs.device.bmp";
				case EPlugInImage.OnlineFamos :			return "imgs.onlinefamos.bmp";
				
				case EPlugInImage.DeleteUser :			return "imgs.user_delete2.bmp";

				case EPlugInImage.Folder :				return "imgs.folder_open16.bmp";
				case EPlugInImage.Folders :				return "imgs.folders.bmp";
				case EPlugInImage.TestCase :			return "imgs.data.bmp";
				case EPlugInImage.TestCaseSelected :	return "imgs.data_sel.bmp";

				case EPlugInImage.Connect :				return "imgs.active_network_connection16.bmp";
				case EPlugInImage.Disconnect :			return "imgs.delete_connection16.bmp";
				case EPlugInImage.Prepare :				return "imgs.prepare.bmp";
				case EPlugInImage.Start :				return "imgs.start.bmp";
				case EPlugInImage.Start2 :				return "imgs.start3.bmp";
				case EPlugInImage.Stop :				return "imgs.stop.bmp";

				case EPlugInImage.Channel :				return "imgs.channel.bmp";

				case EPlugInImage.CharacteristicCurve :	return "imgs.characteristiccurve.bmp";
				case EPlugInImage.CharacteristicCurveSelected :	return "imgs.characteristiccurve_sel.bmp";
				case EPlugInImage.DeletePoint :			return "imgs.point_delete.bmp";

				default: return "";
			}
		}

		public ImageList Images {
			get {
				return m_cImageList;
			}
		}

		public Image GetImage(EPlugInImage eImage) {
			int iImageIndex = -1;
			if (m_htImgIndicies.Contains(eImage))
				iImageIndex = (int) m_htImgIndicies[eImage];

			if (iImageIndex != -1)
				return m_cImageList.Images[iImageIndex];
			else
				return null;
		}

		public int GetImageIndex(EPlugInImage eImage) {
			int iImageIndex = -1;
			if (m_htImgIndicies.Contains(eImage))
				iImageIndex = (int) m_htImgIndicies[eImage];

			return iImageIndex;
		}

		private ImageList m_cImageList = null;
		private Hashtable m_htImgIndicies = null;
	}


}
