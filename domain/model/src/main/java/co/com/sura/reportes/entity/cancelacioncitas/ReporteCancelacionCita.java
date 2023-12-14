package co.com.sura.reportes.entity.cancelacioncitas;

import co.com.sura.maestros.entity.Regional;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;


@Data
@AllArgsConstructor
@NoArgsConstructor
@SuperBuilder(toBuilder = true)
public class ReporteCancelacionCita {
    private Regional regional;
}
