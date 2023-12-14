package co.com.sura.remision.entity.datosremision;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Medicamento {
    private String idMedicamento;
    private String nombre;
    private String presentacion;
    private String codigoMedicamento;
}
