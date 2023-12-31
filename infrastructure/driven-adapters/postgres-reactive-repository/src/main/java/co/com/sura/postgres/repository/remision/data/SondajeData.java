package co.com.sura.postgres.repository.remision.data;

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
@Table(name = "sondaje")
public class SondajeData {
    @Id
    @Column("id_sondaje")
    private Integer idSondaje;

    @Column("tipo_sondaje")
    private String tipoSondaje;

    @Column("sondaje")
    private String sondaje;

    @Column("total_sesiones")
    private Integer totalSesiones;

    @Column("tipo_prestacion")
    private String tipoPrestacion;

    @Column("id_cita")
    private String idCita;
}
