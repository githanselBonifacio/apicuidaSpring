package co.com.sura.entity.remision;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class SoporteNutricional {
    private Medicamento medicamento;
    private Integer cantidadDosis;
    private String unidadDosis;
    private String tipo;
    private String descripcion;
    private Integer duracion;
    private Integer volumen;
    private boolean noPBS;
    private String tipoPrestacion;
}
