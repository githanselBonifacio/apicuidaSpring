package co.com.sura.entity.reportes.cancelacioncitas;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@AllArgsConstructor
@NoArgsConstructor
@SuperBuilder(toBuilder = true)
public class RegistroCancelacionCita {
    private String descripcion;
    private int cantidad;
}
