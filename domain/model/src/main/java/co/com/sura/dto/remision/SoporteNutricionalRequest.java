package co.com.sura.dto.remision;

import co.com.sura.entity.remision.Medicamento;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class SoporteNutricionalRequest {
    private Medicamento medicamento;
    private Integer cantidadDosis;
    private UnidadDosisRequest unidadDosis;
    private TipoNutricionRequest tipoNutricion;
    private String tipo;
    private String idTipo;
    private String idNutricion;
    private String descripcion;
    private Integer duracion;
    private Integer volumen;
    private boolean noPBS;
    private List<Object> eventos;
    private String tipoPrestacion;
}
