package co.com.sura.reportes.entity.cancelacioncitas;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import java.util.Collection;


@Data
@AllArgsConstructor
@NoArgsConstructor
@SuperBuilder(toBuilder = true)
public class RegistroCancelacionCita {
    private String descripcion;
    private Integer cantidad;

    public static Integer calcularTotalCantidad(Collection<RegistroCancelacionCita> registros){
        return registros.stream()
                .mapToInt(RegistroCancelacionCita::getCantidad)
                .sum();

    }
}
