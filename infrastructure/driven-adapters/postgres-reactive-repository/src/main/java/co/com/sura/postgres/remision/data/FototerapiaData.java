package co.com.sura.postgres.remision.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "fototerapias")
public class FototerapiaData {
    @Id
    @Column("id_fototerapia")
    private Integer idFototerapia;

    @Column("dias_tratamiento")
    private Integer diasTratamiento;

    @Column("tipo_frecuencia")
    private String tipoFrecuencia;

    @Column("cantidad_dosis")
    private Integer cantidadDosis;

    @Column("tipo_prestacion")
    private String tipoPrestacion;

    @Column("id_cita")
    private String idCita;
}
