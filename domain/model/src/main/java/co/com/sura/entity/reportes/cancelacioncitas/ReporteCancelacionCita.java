package co.com.sura.entity.reportes.cancelacioncitas;

import co.com.sura.entity.maestro.Regional;
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
